{-# LANGUAGE RecordWildCards #-}

module Ext.Test.Result.Report
  ( renderReports
  , renderReport
  , renderReportsWithDuration
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
  renderReportsWithDuration True Nothing Nothing Nothing reports

-- Render with an optional duration (in milliseconds), seed, fuzz, and a color toggle.
-- When any test has failed, we show only the failures in the body; otherwise, we show just the summary.
renderReportsWithDuration :: Bool -> Maybe Int -> Maybe Int -> Maybe Int -> [R.Report] -> String
renderReportsWithDuration useColors maybeDuration maybeSeed maybeFuzz reports =
  let fns = if useColors then defaultColorFns else plainColorFns
      allResults = concatMap (concatMap R.testRunResult . R.reportRuns) reports
      anyFailed = any isFail allResults
      summary = renderHeaderWith fns maybeDuration maybeSeed maybeFuzz reports
      body =
        if anyFailed
          then
            let onlyFailures r =
                  let runsFiltered =
                        [ run { R.testRunResult = filter isFail (R.testRunResult run) }
                        | run <- R.reportRuns r
                        , any isFail (R.testRunResult run)
                        ]
                  in r { R.reportRuns = runsFiltered }
                nonEmpty r = not (null (R.reportRuns r))
                failingReports = filter nonEmpty (fmap onlyFailures reports)
            in Data.List.intercalate "\n\n" (fmap (renderReportWith fns) failingReports)
          else ""
  in if null body then summary else body ++ "\n\n" ++ summary


renderReport :: R.Report -> String
renderReport R.Report{..} =
  let header = reportId ++ onlySuffix reportIsOnly
      items = fmap (\R.TestRun{..} -> (testRunLabel, testRunResult)) reportRuns
  in renderGroup (header, items)

-- Color-parameterized rendering
renderReportWith :: ColorFns -> R.Report -> String
renderReportWith fns R.Report{..} =
  let header = reportId ++ onlySuffixWith fns reportIsOnly
      items = fmap (\R.TestRun{..} -> (testRunLabel, testRunResult)) reportRuns
      -- Check if we should render failures with details
      allResults = concatMap snd items
      hasFailures = any isFail allResults
  in if hasFailures
       then renderFailuresWithPath fns header items
       else renderGroupWith fns (header, items)

-- Render failures with their full paths and details
renderFailuresWithPath :: ColorFns -> String -> [([String], [R.TestResult])] -> String
renderFailuresWithPath fns reportHeader items =
  let -- Extract base name from report header (e.g., "Header.suite" -> "Header")
      baseHeader = case Data.List.elemIndex '.' reportHeader of
        Just idx -> take idx reportHeader
        Nothing -> reportHeader
      failures = concatMap (\(labels, results) ->
                              [ (labels, result) | result <- results, isFail result ]
                           ) items
      renderFailure (labels, R.Failed{..}) =
        let -- Build path: report header, then all label segments except last
            (pathSegments, testName) = case labels of
              [] -> ([baseHeader], "(no label)")
              [single] -> ([baseHeader], single)
              multiple -> (baseHeader : init multiple, last multiple)
            pathLines = map (\label -> dim' fns ("↓ " ++ label)) pathSegments
            failureHeader = red' fns ("✗ " ++ testName)
            failureDetails = renderFailureDetails fns failureMessage failureReason
        in Data.List.intercalate "\n" (pathLines ++ [failureHeader, ""] ++ [failureDetails])
      renderFailure _ = "" -- Should not happen due to filter
  in Data.List.intercalate "\n\n\n" (map renderFailure failures)

-- Render failure details in a format similar to elm-test
renderFailureDetails :: ColorFns -> String -> R.Reason -> String
renderFailureDetails fns message reason =
  case reason of
    R.ListDiff{..} ->
      let expectedStr = "[" ++ Data.List.intercalate ", " (map showQuoted listDiffExpected) ++ "]"
          actualStr = "[" ++ Data.List.intercalate ", " (map showQuoted listDiffActual) ++ "]"
      in indent 4 ("Expected " ++ expectedStr ++ ", got: " ++ actualStr)
    R.Equality{..} ->
      -- Format as list with single element: ["value"]
      -- Check if the string is already formatted as a list, otherwise format it
      let formatValue s = 
            if not (null s) && head s == '[' && not (null s) && last s == ']'
              then s  -- Already formatted as a list
              else "[" ++ showQuoted s ++ "]"  -- Format as list with quoted string
          expectedLine = indent 4 (formatValue equalityExpected)
          separator = indent 4 "╷"
          messageLine = indent 4 ("│ " ++ message)
          actualLine = indent 4 "╵"
          actualValue = indent 4 (formatValue equalityActual)
      in expectedLine ++ "\n" ++ separator ++ "\n" ++ messageLine ++ "\n" ++ actualLine ++ "\n" ++ actualValue
    R.Comparison{..} ->
      let firstLine = indent 4 (showQuoted comparisonFirst)
          separator = indent 4 "╷"
          messageLine = indent 4 ("│ " ++ message)
          secondLine = indent 4 "╵"
          secondValue = indent 4 (showQuoted comparisonSecond)
      in firstLine ++ "\n" ++ separator ++ "\n" ++ messageLine ++ "\n" ++ secondLine ++ "\n" ++ secondValue
    R.CollectionDiff{..} ->
      let expectedLine = indent 4 ("Expected: " ++ showQuoted collectionExpected)
          actualLine = indent 4 ("Actual: " ++ showQuoted collectionActual)
          extraLines = if null collectionExtra
                       then ""
                       else "\n" ++ indent 4 ("Extra: " ++ Data.List.intercalate ", " (map showQuoted collectionExtra))
          missingLines = if null collectionMissing
                         then ""
                         else "\n" ++ indent 4 ("Missing: " ++ Data.List.intercalate ", " (map showQuoted collectionMissing))
      in expectedLine ++ "\n" ++ actualLine ++ extraLines ++ missingLines
    _ ->
      -- For other reason types, use the existing renderReasonWith format
      renderReasonWith fns 4 reason

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

renderGroupWith :: ColorFns -> (String, [([String], [R.TestResult])]) -> String
renderGroupWith fns (groupLabel, items) =
  let (selfs, nonSelfs) = foldr (\(p, rs) (ss, ns) -> if null p then (rs:ss, ns) else (ss, (p, rs):ns)) ([], []) items
      headerLine = bold' fns groupLabel
      selfLine =
        if null selfs then ""
        else let (statusStr, colorFn) = summarizeStatusWith fns (concat selfs)
             in "\n" ++ indent 2 (groupLabel ++ " . " ++ colorFn statusStr)
      children = renderChildrenWith fns 2 nonSelfs
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

renderChildrenWith :: ColorFns -> Int -> [([String], [R.TestResult])] -> String
renderChildrenWith fns headerIndent items =
  let groups = groupByFirstSegment items
  in Data.List.intercalate "\n" (map (renderKeyGroupWith fns headerIndent) (Data.List.sortOn fst groups))

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

renderLeafLineWith :: ColorFns -> Int -> Int -> (String, [R.TestResult]) -> String
renderLeafLineWith fns leafIndent maxLeafLen (name, rs) =
  let (statusStr, colorFn) = summarizeStatusWith fns rs
      dots = replicate (max 1 (maxLeafLen - length name + 1)) '.'
  in indent leafIndent (name ++ " " ++ dots ++ " " ++ colorFn statusStr)

-- Render all leaves, sorted by name, or an empty string if none.
renderLeavesBlock :: Int -> [(String, [R.TestResult])] -> String
renderLeavesBlock leafIndent mergedLeaves =
  if null mergedLeaves then ""
  else
    let maxLeafLen = computeMaxLeafNameLength mergedLeaves
    in Data.List.intercalate "\n" (map (renderLeafLine leafIndent maxLeafLen) (Data.List.sortOn fst mergedLeaves))

renderLeavesBlockWith :: ColorFns -> Int -> [(String, [R.TestResult])] -> String
renderLeavesBlockWith fns leafIndent mergedLeaves =
  if null mergedLeaves then ""
  else
    let maxLeafLen = computeMaxLeafNameLength mergedLeaves
    in Data.List.intercalate "\n" (map (renderLeafLineWith fns leafIndent maxLeafLen) (Data.List.sortOn fst mergedLeaves))

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

renderSubtreesBlockWith :: ColorFns -> Int -> [(String, [([String], [R.TestResult])])] -> String
renderSubtreesBlockWith fns headerIndent branchWithChildren =
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
                    leavesBlockChild = renderLeavesBlockWith fns (headerIndent + 4) mergedLeavesChild
                    subtreeBlockChild = renderSubtreesBlockWith fns (headerIndent + 2) branchWithChildrenChild
                    below = filter (not . null) [leavesBlockChild, subtreeBlockChild]
                in subHeader ++ (if null below then "" else "\n" ++ Data.List.intercalate "\n" below)
           else renderSelfLineForKeyWith fns (headerIndent + 2) n onlySelfRs


-- If there are self results at this key (exact label), render a summary line named by the key.
renderSelfLineForKey :: Int -> String -> [[R.TestResult]] -> String
renderSelfLineForKey leafIndent key selfLeaves =
  if null selfLeaves then ""
  else let (statusStr, colorFn) = summarizeStatus (concat selfLeaves)
       in indent leafIndent (key ++ " . " ++ colorFn statusStr)

renderSelfLineForKeyWith :: ColorFns -> Int -> String -> [[R.TestResult]] -> String
renderSelfLineForKeyWith fns leafIndent key selfLeaves =
  if null selfLeaves then ""
  else let (statusStr, colorFn) = summarizeStatusWith fns (concat selfLeaves)
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

renderKeyGroupWith :: ColorFns -> Int -> (String, [([String], [R.TestResult])]) -> String
renderKeyGroupWith fns headerIndent (key, children) =
  let headerLine = indent headerIndent key
      selfLeaves = [ rs | ([], rs) <- children ]
      (leaves, branches) = splitLeavesAndBranches children
      branchGroups = groupBranchesByNextSegment branches
      (branchOnlySelf, branchWithChildren0) = partitionBranchGroupsByChildren branchGroups
      (redundantAsLeaf, branchWithChildren) = collapseRedundantSingleLeafBranches branchWithChildren0
      mergedLeaves = mergeLeaves leaves branchOnlySelf redundantAsLeaf
      leavesBlock = renderLeavesBlockWith fns (headerIndent + 2) mergedLeaves
      subtree = renderSubtreesBlockWith fns headerIndent branchWithChildren
      selfLine = renderSelfLineForKeyWith fns (headerIndent + 2) key selfLeaves
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

summarizeStatusWith :: ColorFns -> [R.TestResult] -> (String, String -> String)
summarizeStatusWith fns results =
  if any isFail results then ("Fail", red' fns)
  else if any isSkip results then ("Skip", yellow' fns)
  else ("Pass", green' fns)

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

renderResultWith :: ColorFns -> Int -> R.TestResult -> String
renderResultWith fns n result =
  case result of
    R.Passed -> indent n (green' fns "✔ Passed")
    R.Skipped -> indent n (yellow' fns "⊝ Skipped")
    R.Failed{..} ->
      let header = indent n (red' fns "✖ Failed" ++
                              (case failureGiven of
                                 Just g  -> dim' fns ("  (given " ++ showQuoted g ++ ")")
                                 Nothing -> "")
                             )
          msg = indent (n + 2) (bold' fns failureMessage)
          detail = renderReasonWith fns (n + 2) failureReason
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

renderReasonWith :: ColorFns -> Int -> R.Reason -> String
renderReasonWith fns n reason =
  case reason of
    R.Custom -> indent n (dim' fns "Reason: custom")
    R.TODO -> indent n (yellow' fns "TODO")
    R.Invalid s -> indent n (red' fns ("Invalid: " ++ s))
    R.Equality{..} ->
      indent n (cyan' fns "Expected:" ) ++ "\n" ++
      indent (n + 2) (green' fns equalityExpected) ++ "\n" ++
      indent n (cyan' fns "Actual:") ++ "\n" ++
      indent (n + 2) (red' fns equalityActual)
    R.Comparison{..} ->
      indent n (cyan' fns "First:" ) ++ "\n" ++
      indent (n + 2) (green' fns comparisonFirst) ++ "\n" ++
      indent n (cyan' fns "Second:") ++ "\n" ++
      indent (n + 2) (red' fns comparisonSecond)
    R.ListDiff{..} ->
      indent n (cyan' fns "Expected list:") ++ formatStringList (n + 2) listDiffExpected ++
      indent n (cyan' fns "Actual list:")   ++ formatStringList (n + 2) listDiffActual
    R.CollectionDiff{..} ->
      indent n (cyan' fns "Expected:") ++ "\n" ++ indent (n + 2) (green' fns collectionExpected) ++ "\n" ++
      indent n (cyan' fns "Actual:"  ) ++ "\n" ++ indent (n + 2) (red' fns collectionActual) ++
      (if null collectionExtra then "" else "\n" ++ indent n (cyan' fns "Extra:")   ++ formatStringList (n + 2) collectionExtra) ++
      (if null collectionMissing then "" else "\n" ++ indent n (cyan' fns "Missing:") ++ formatStringList (n + 2) collectionMissing)

renderHeader :: Maybe Int -> [R.Report] -> String
renderHeader maybeDuration reports =
  let allResults = concatMap (concatMap R.testRunResult . R.reportRuns) reports
      total = length allResults
      passed = length (filter isPass allResults)
      failed = length (filter isFail allResults)
      skipped = length (filter isSkip allResults)
      _onlyAny = or (fmap R.reportIsOnly reports)
      statusLine =
        if failed > 0
          then bold (red "TEST RUN FAILED")
          else bold (green "TEST RUN PASSED")
      durationPart =
        case maybeDuration of
          Nothing -> []
          Just ms -> [ "Duration: " ++ show ms ++ " ms" ]
      skippedPart =
        if skipped > 0 then [ "Skipped:  " ++ show skipped ] else []
      passedPart = [ "Passed:   " ++ show passed ]
      failedPart = [ "Failed:   " ++ show failed ]
      linesOut =
        [ ""
        , statusLine
        , ""
        ] ++
        (durationPart ++ passedPart ++ failedPart ++ skippedPart)
      _unusedTotal = total
  in Data.List.intercalate "\n" linesOut

renderHeaderWith :: ColorFns -> Maybe Int -> Maybe Int -> Maybe Int -> [R.Report] -> String
renderHeaderWith fns maybeDuration maybeSeed maybeFuzz reports =
  let allResults = concatMap (concatMap R.testRunResult . R.reportRuns) reports
      total = length allResults
      passed = length (filter isPass allResults)
      failed = length (filter isFail allResults)
      skipped = length (filter isSkip allResults)
      _onlyAny = or (fmap R.reportIsOnly reports)
      statusLine =
        if failed > 0
          then bold' fns (underline' fns (red' fns "TEST RUN FAILED"))
          else bold' fns (green' fns "TEST RUN PASSED")
      durationPart =
        case maybeDuration of
          Nothing -> []
          Just ms -> [ "Duration: " ++ show ms ++ " ms" ]
      skippedPart =
        if skipped > 0 then [ "Skipped:  " ++ show skipped ] else []
      passedPart = [ "Passed:   " ++ show passed ]
      failedPart = [ "Failed:   " ++ show failed ]
      reproductionPart =
        case (maybeSeed, maybeFuzz) of
          (Just seed, Just fuzz) ->
            [ "Running " ++ show total ++ " tests. To reproduce these results, run: elm-test --fuzz " ++ show fuzz ++ " --seed " ++ show seed ]
          _ -> []
      linesOut =
        reproductionPart ++
        (if not (null reproductionPart) then [ "" ] else []) ++
        [ ""
        , statusLine
        , ""
        ] ++
        (durationPart ++ passedPart ++ failedPart ++ skippedPart)
      _unusedTotal = total
  in Data.List.intercalate "\n" linesOut


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

onlySuffixWith :: ColorFns -> Bool -> String
onlySuffixWith fns isOnly = if isOnly then " " ++ dim' fns ("(" ++ cyan' fns "only" ++ ")") else ""

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

underline :: String -> String
underline s = "\ESC[4m" ++ s ++ "\ESC[0m"

-- Color parameterization
data ColorFns =
  ColorFns
    { boldF :: String -> String
    , dimF :: String -> String
    , redF :: String -> String
    , greenF :: String -> String
    , yellowF :: String -> String
    , cyanF :: String -> String
    , underlineF :: String -> String
    }

defaultColorFns :: ColorFns
defaultColorFns =
  ColorFns { boldF = bold, dimF = dim, redF = red, greenF = green, yellowF = yellow, cyanF = cyan, underlineF = underline }

plainColorFns :: ColorFns
plainColorFns =
  ColorFns { boldF = id, dimF = id, redF = id, greenF = id, yellowF = id, cyanF = id, underlineF = id }

bold' :: ColorFns -> String -> String
bold' fns = boldF fns

dim' :: ColorFns -> String -> String
dim' fns = dimF fns

red' :: ColorFns -> String -> String
red' fns = redF fns

green' :: ColorFns -> String -> String
green' fns = greenF fns

yellow' :: ColorFns -> String -> String
yellow' fns = yellowF fns

cyan' :: ColorFns -> String -> String
cyan' fns = cyanF fns

underline' :: ColorFns -> String -> String
underline' fns = underlineF fns