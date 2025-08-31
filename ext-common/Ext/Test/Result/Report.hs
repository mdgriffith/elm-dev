{-# LANGUAGE RecordWildCards #-}

module Ext.Test.Result.Report
  ( renderReports
  , renderReport
  , printReports
  ) where

import qualified Data.List
import qualified Ext.Common
import qualified Ext.Test.Result as R


-- Public rendering API

printReports :: [R.Report] -> IO ()
printReports reports =
  Ext.Common.atomicPutStrLn (renderReports reports)


renderReports :: [R.Report] -> String
renderReports reports =
  let header = renderHeader reports
      body = Data.List.intercalate "\n\n" (fmap renderReport reports)
  in header ++ "\n\n" ++ body


renderReport :: R.Report -> String
renderReport R.Report{..} =
  let header = reportId ++ onlySuffix reportIsOnly
      items = fmap (\R.TestRun{..} -> (testRunLabel, testRunResult)) reportRuns
  in renderGroup (header, items)


-- Internal helpers

renderRun :: R.TestRun -> String
renderRun R.TestRun{..} =
  let labelsPath = if null testRunLabel then dim "(no label)" else dim (Data.List.intercalate " › " testRunLabel)
      path = bullet ++ " " ++ labelsPath
      results = Data.List.intercalate "\n" (fmap (renderResult 4) testRunResult)
  in indent 2 path ++ (if null results then "" else "\n" ++ results)


-- Grouped rendering

renderGroup :: (String, [([String], [R.TestResult])]) -> String
renderGroup (groupLabel, items) =
  let (selfs, nonSelfs) = foldr (\(p, rs) (ss, ns) -> if null p then (rs:ss, ns) else (ss, (p, rs):ns)) ([], []) items
      headerLine = bold groupLabel
      selfLine = if null selfs then "" else let (statusStr, colorFn) = summarizeStatus (concat selfs)
                                             in "\n" ++ indent 2 (groupLabel ++ " . " ++ colorFn statusStr)
      children = renderChildren 2 nonSelfs
  in headerLine ++ selfLine ++ (if null children then "" else "\n" ++ children)


-- Render children of a group by their first segment. For each first-segment key:
-- - If there are entries with empty tail (i.e., exactly the key), render a leaf line named by the key.
-- - If there are entries with one remaining segment, render those as leaf lines under the key header.
-- - If there are entries with two or more remaining segments, render a sub-header for the next segment and recurse.
--
-- This function now delegates to smaller helpers below to keep the high-level flow easy to follow.
renderChildren :: Int -> [([String], [R.TestResult])] -> String
renderChildren headerIndent items =
  let groups = groupByFirstSegment items
      renderKeyGroup' = renderKeyGroup headerIndent
  in Data.List.intercalate "\n" (map renderKeyGroup' (Data.List.sortOn fst groups))


-- Group items by their first label segment, keeping the remaining tail with results.
-- Example: ( ["A","B"], rs ) contributes to key "A" with child ( ["B"], rs ).
groupByFirstSegment :: [([String], [R.TestResult])] -> [(String, [([String], [R.TestResult])])]
groupByFirstSegment items =
  let insertGroup (path, rs) acc = case path of
        []     -> acc
        (x:xs) -> case lookup x acc of
          Nothing -> (x, [(xs, rs)]) : acc
          Just ys -> (x, (xs, rs) : ys) : filter ((/= x) . fst) acc
  in foldr insertGroup [] items


-- For a given key's children, split into:
-- - leaves: exactly one remaining segment [name]
-- - branches: two or more remaining segments (name:rest)
-- Entries with empty path ([]) are excluded here; handle them as "self" elsewhere.
splitLeavesAndBranches :: [([String], [R.TestResult])] -> ([(String, [R.TestResult])], [([String], [R.TestResult])])
splitLeavesAndBranches children =
  foldr step ([], []) children
  where
    step (p, rs) (ls, bs) = case p of
      [n]   -> ((n, rs) : ls, bs)
      (n:xs) -> (ls, ((n:xs), rs) : bs)
      []    -> (ls, bs)


-- Group branch entries by their next segment name.
-- Example: (["B","C"], rs) groups under key "B" with child (["C"], rs).
groupBranchesByNextSegment :: [([String], [R.TestResult])] -> [(String, [([String], [R.TestResult])])]
groupBranchesByNextSegment branches =
  let insert (p, rs) acc = case p of
        (n:xs) -> case lookup n acc of
          Nothing -> (n, [(xs, rs)]) : acc
          Just ys -> (n, (xs, rs) : ys) : filter ((/= n) . fst) acc
        [] -> acc
  in foldr insert [] branches


-- Partition grouped branches into:
-- - onlySelf: all children are empty tails
-- - withChildren: at least one child has a non-empty tail
partitionBranchGroupsByChildren :: [(String, [([String], [R.TestResult])])] -> ([(String, [([String], [R.TestResult])])], [(String, [([String], [R.TestResult])])])
partitionBranchGroupsByChildren branchGroups =
  Data.List.partition (\(_, ch) -> all (\(p, _) -> null p) ch) branchGroups


-- Collapse branches that only contain a single leaf child with the same name as the branch key.
-- These can be rendered as a single leaf line at the current level.
collapseRedundantSingleLeafBranches :: [(String, [([String], [R.TestResult])])] -> ([(String, [R.TestResult])], [(String, [([String], [R.TestResult])])])
collapseRedundantSingleLeafBranches groups =
  foldr step ([], []) groups
  where
    isRedundant (n, ch) = not (null ch) && all (\(p, _) -> p == [n]) ch
    step (n, ch) (rs, bs) =
      if isRedundant (n, ch)
        then ((n, concatMap snd ch) : rs, bs)
        else (rs, (n, ch) : bs)


-- Merge leaf sources: explicit leaves, branch groups that only contain "self", and redundant branches collapsed to leaves.
mergeLeaves :: [(String, [R.TestResult])] -> [(String, [([String], [R.TestResult])])] -> [(String, [R.TestResult])] -> [(String, [R.TestResult])]
mergeLeaves leaves branchOnlySelf redundantAsLeaf =
  leaves ++ [ (n, concatMap snd ch) | (n, ch) <- branchOnlySelf ] ++ redundantAsLeaf


-- Compute maximum leaf name length to align dotted status columns.
computeMaxLeafNameLength :: [(String, [R.TestResult])] -> Int
computeMaxLeafNameLength mergedLeaves =
  let leafNames = map fst mergedLeaves
  in foldr max 0 (map length leafNames)


-- Render a single aligned leaf line with status.
renderLeafLine :: Int -> Int -> (String, [R.TestResult]) -> String
renderLeafLine leafIndent maxLeafLen (name, rs) =
  let (statusStr, colorFn) = summarizeStatus rs
      dots = replicate (max 1 (maxLeafLen - length name + 1)) '.'
  in indent leafIndent (name ++ " " ++ dots ++ " " ++ colorFn statusStr)


-- Render all leaves, sorted by name, or an empty string if none.
renderLeavesBlock :: Int -> [(String, [R.TestResult])] -> String
renderLeavesBlock leafIndent mergedLeaves =
  if null mergedLeaves then ""
  else
    let maxLeafLen = computeMaxLeafNameLength mergedLeaves
    in Data.List.intercalate "\n" (map (renderLeafLine leafIndent maxLeafLen) (Data.List.sortOn fst mergedLeaves))


-- Render nested subtrees under their sub-headers for groups that still have children.
renderSubtreesBlock :: Int -> [(String, [([String], [R.TestResult])])] -> String
renderSubtreesBlock headerIndent branchWithChildren =
  if null branchWithChildren then ""
  else Data.List.intercalate "\n" (map renderOne (Data.List.sortOn fst branchWithChildren))
  where
    renderOne (n, ch) =
      let (leavesChild, branchesChild) = splitLeavesAndBranches ch
          branchGroupsChild = groupBranchesByNextSegment branchesChild
          (branchOnlySelfChild, branchWithChildrenChild0) = partitionBranchGroupsByChildren branchGroupsChild
          (redundantAsLeafChild, branchWithChildrenChild) = collapseRedundantSingleLeafBranches branchWithChildrenChild0
          mergedLeavesChild = mergeLeaves leavesChild branchOnlySelfChild redundantAsLeafChild
          hasChildrenBlocksChild = not (null mergedLeavesChild) || not (null branchWithChildrenChild)
          onlySelfRs = [ rs | (p, rs) <- ch, null p ]
      in if hasChildrenBlocksChild
           then let subHeader = indent (headerIndent + 2) n
                    leavesBlockChild = renderLeavesBlock (headerIndent + 4) mergedLeavesChild
                    subtreeBlockChild = renderSubtreesBlock (headerIndent + 2) branchWithChildrenChild
                    below = filter (not . null) [leavesBlockChild, subtreeBlockChild]
                in subHeader ++ (if null below then "" else "\n" ++ Data.List.intercalate "\n" below)
           else renderSelfLineForKey (headerIndent + 2) n onlySelfRs


-- If there are self results at this key (exact label), render a summary line named by the key.
renderSelfLineForKey :: Int -> String -> [[R.TestResult]] -> String
renderSelfLineForKey leafIndent key selfLeaves =
  if null selfLeaves then ""
  else let (statusStr, colorFn) = summarizeStatus (concat selfLeaves)
       in indent leafIndent (key ++ " . " ++ colorFn statusStr)


-- Render a single first-segment group (key and its children) using the helpers above.
renderKeyGroup :: Int -> (String, [([String], [R.TestResult])]) -> String
renderKeyGroup headerIndent (key, children) =
  let headerLine = indent headerIndent key
      selfLeaves = [ rs | ([], rs) <- children ]
      (leaves, branches) = splitLeavesAndBranches children
      branchGroups = groupBranchesByNextSegment branches
      (branchOnlySelf, branchWithChildren0) = partitionBranchGroupsByChildren branchGroups
      (redundantAsLeaf, branchWithChildren) = collapseRedundantSingleLeafBranches branchWithChildren0
      mergedLeaves = mergeLeaves leaves branchOnlySelf redundantAsLeaf
      leavesBlock = renderLeavesBlock (headerIndent + 2) mergedLeaves
      subtree = renderSubtreesBlock headerIndent branchWithChildren
      selfLine = renderSelfLineForKey (headerIndent + 2) key selfLeaves
      hasChildrenBlocks = not (null subtree) || not (null leavesBlock)
      parts = if hasChildrenBlocks
                then filter (not . null) [headerLine, subtree, leavesBlock, selfLine]
                else filter (not . null) [selfLine]
  in Data.List.intercalate "\n" parts


summarizeStatus :: [R.TestResult] -> (String, String -> String)
summarizeStatus results =
  if any isFail results then ("Fail", red)
  else if any isSkip results then ("Skip", yellow)
  else ("Pass", green)


groupRunsByHeadLabel :: [R.TestRun] -> [(String, [([String], [R.TestResult])])]
groupRunsByHeadLabel runs =
  let toPair r =
        let labels = R.testRunLabel r
            headLabel = case labels of { [] -> "(no label)"; (x:_) -> x }
            tailSegments = case labels of { [] -> []; (_:xs) -> xs }
        in (headLabel, (tailSegments, R.testRunResult r))
      insertPair (k, v) acc = case lookup k acc of
        Nothing -> (k, [v]) : acc
        Just vs -> (k, v : vs) : filter ((/= k) . fst) acc
  in foldr insertPair [] (map toPair runs)


renderResult :: Int -> R.TestResult -> String
renderResult n result =
  case result of
    R.Passed -> indent n (green "✔ Passed")
    R.Skipped -> indent n (yellow "⊝ Skipped")
    R.Failed{..} ->
      let header = indent n (red "✖ Failed" ++
                              (case failureGiven of
                                 Just g  -> dim ("  (given " ++ showQuoted g ++ ")")
                                 Nothing -> "")
                             )
          msg = indent (n + 2) (bold failureMessage)
          detail = renderReason (n + 2) failureReason
      in header ++ "\n" ++ msg ++ (if null detail then "" else "\n" ++ detail)


renderReason :: Int -> R.Reason -> String
renderReason n reason =
  case reason of
    R.Custom -> indent n (dim "Reason: custom")
    R.TODO -> indent n (yellow "TODO")
    R.Invalid s -> indent n (red ("Invalid: " ++ s))
    R.Equality{..} ->
      indent n (cyan "Expected:" ) ++ "\n" ++
      indent (n + 2) (green equalityExpected) ++ "\n" ++
      indent n (cyan "Actual:") ++ "\n" ++
      indent (n + 2) (red equalityActual)
    R.Comparison{..} ->
      indent n (cyan "First:" ) ++ "\n" ++
      indent (n + 2) (green comparisonFirst) ++ "\n" ++
      indent n (cyan "Second:") ++ "\n" ++
      indent (n + 2) (red comparisonSecond)
    R.ListDiff{..} ->
      indent n (cyan "Expected list:") ++ formatStringList (n + 2) listDiffExpected ++
      indent n (cyan "Actual list:")   ++ formatStringList (n + 2) listDiffActual
    R.CollectionDiff{..} ->
      indent n (cyan "Expected:") ++ "\n" ++ indent (n + 2) (green collectionExpected) ++ "\n" ++
      indent n (cyan "Actual:"  ) ++ "\n" ++ indent (n + 2) (red collectionActual) ++
      (if null collectionExtra then "" else "\n" ++ indent n (cyan "Extra:")   ++ formatStringList (n + 2) collectionExtra) ++
      (if null collectionMissing then "" else "\n" ++ indent n (cyan "Missing:") ++ formatStringList (n + 2) collectionMissing)


renderHeader :: [R.Report] -> String
renderHeader reports =
  let allResults = concatMap (concatMap R.testRunResult . R.reportRuns) reports
      total = length allResults
      passed = length (filter isPass allResults)
      failed = length (filter isFail allResults)
      skipped = length (filter isSkip allResults)
      onlyAny = or (fmap R.reportIsOnly reports)
      summaryParts =
        [ bold ("Summary: " ++ (if onlyAny then cyan "(only) " else ""))
        , green (show passed ++ " passed")
        , red (show failed ++ " failed")
        , yellow (show skipped ++ " skipped")
        , dim ("(" ++ show total ++ " total)")
        ]
  in Data.List.intercalate "  " summaryParts


isPass :: R.TestResult -> Bool
isPass r = case r of { R.Passed -> True; _ -> False }

isFail :: R.TestResult -> Bool
isFail r = case r of { R.Failed{} -> True; _ -> False }

isSkip :: R.TestResult -> Bool
isSkip r = case r of { R.Skipped -> True; _ -> False }


formatStringList :: Int -> [String] -> String
formatStringList n items =
  if null items
    then "\n" ++ indent n (dim "(none)")
    else "\n" ++ Data.List.intercalate "\n" (fmap (indent n . (\s -> "- " ++ s)) items)


onlySuffix :: Bool -> String
onlySuffix isOnly = if isOnly then " " ++ dim ("(" ++ cyan "only" ++ ")") else ""


-- Pretty helpers

indent :: Int -> String -> String
indent n str = replicate n ' ' ++ str

bullet :: String
bullet = "•"

showQuoted :: String -> String
showQuoted s = "\"" ++ s ++ "\""

-- ANSI helpers

bold :: String -> String
bold s = "\ESC[1m" ++ s ++ "\ESC[0m"

dim :: String -> String
dim s = "\ESC[2m" ++ s ++ "\ESC[0m"

red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"

yellow :: String -> String
yellow s = "\ESC[33m" ++ s ++ "\ESC[0m"

cyan :: String -> String
cyan s = "\ESC[36m" ++ s ++ "\ESC[0m"