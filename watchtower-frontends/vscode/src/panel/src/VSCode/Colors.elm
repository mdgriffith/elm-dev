module VSCode.Colors exposing (..)

{-| This file is generated via a command from elm-dev!
-}

import Element
import Html.Attributes as Attr


stylesheet : String
stylesheet =
    """.foreground { color: var(--vscode-foreground) !important; }.disabledForeground { color: var(--vscode-disabledForeground) !important; }.errorForeground { color: var(--vscode-errorForeground) !important; }.descriptionForeground { color: var(--vscode-descriptionForeground) !important; }.icon-foreground { color: var(--vscode-icon-foreground) !important; }.focusBorder { border-color:  var(--vscode-focusBorder) !important; }.contrastBorder { border-color:  var(--vscode-contrastBorder) !important; }.contrastActiveBorder { border-color:  var(--vscode-contrastActiveBorder) !important; }.selection-background { background-color:  var(--vscode-selection-background) !important; }.textSeparator-foreground { color: var(--vscode-textSeparator-foreground) !important; }.textLink-foreground { color: var(--vscode-textLink-foreground) !important; }.textLink-activeForeground { color: var(--vscode-textLink-activeForeground) !important; }.textPreformat-foreground { color: var(--vscode-textPreformat-foreground) !important; }.textBlockQuote-background { background-color:  var(--vscode-textBlockQuote-background) !important; }.textBlockQuote-border { border-color:  var(--vscode-textBlockQuote-border) !important; }.textCodeBlock-background { background-color:  var(--vscode-textCodeBlock-background) !important; }.input-background { background-color:  var(--vscode-input-background) !important; }.input-foreground { color: var(--vscode-input-foreground) !important; }.input-border { border-color:  var(--vscode-input-border) !important; }.inputOption-activeBorder { border-color:  var(--vscode-inputOption-activeBorder) !important; }.inputOption-hoverBackground { background-color:  var(--vscode-inputOption-hoverBackground) !important; }.inputOption-activeBackground { background-color:  var(--vscode-inputOption-activeBackground) !important; }.inputOption-activeForeground { color: var(--vscode-inputOption-activeForeground) !important; }.input-placeholderForeground { color: var(--vscode-input-placeholderForeground) !important; }.inputValidation-infoBackground { background-color:  var(--vscode-inputValidation-infoBackground) !important; }.inputValidation-infoForeground { color: var(--vscode-inputValidation-infoForeground) !important; }.inputValidation-infoBorder { border-color:  var(--vscode-inputValidation-infoBorder) !important; }.inputValidation-warningBackground { background-color:  var(--vscode-inputValidation-warningBackground) !important; }.inputValidation-warningForeground { color: var(--vscode-inputValidation-warningForeground) !important; }.inputValidation-warningBorder { border-color:  var(--vscode-inputValidation-warningBorder) !important; }.inputValidation-errorBackground { background-color:  var(--vscode-inputValidation-errorBackground) !important; }.inputValidation-errorForeground { color: var(--vscode-inputValidation-errorForeground) !important; }.inputValidation-errorBorder { border-color:  var(--vscode-inputValidation-errorBorder) !important; }.dropdown-background { background-color:  var(--vscode-dropdown-background) !important; }.dropdown-listBackground { background-color:  var(--vscode-dropdown-listBackground) !important; }.dropdown-foreground { color: var(--vscode-dropdown-foreground) !important; }.dropdown-border { border-color:  var(--vscode-dropdown-border) !important; }.button-foreground { color: var(--vscode-button-foreground) !important; }.button-background { background-color:  var(--vscode-button-background) !important; }.button-hoverBackground { background-color:  var(--vscode-button-hoverBackground) !important; }.button-border { border-color:  var(--vscode-button-border) !important; }.button-secondaryForeground { color: var(--vscode-button-secondaryForeground) !important; }.button-secondaryBackground { background-color:  var(--vscode-button-secondaryBackground) !important; }.button-secondaryHoverBackground { background-color:  var(--vscode-button-secondaryHoverBackground) !important; }.badge-background { background-color:  var(--vscode-badge-background) !important; }.badge-foreground { color: var(--vscode-badge-foreground) !important; }.scrollbarSlider-background { background-color:  var(--vscode-scrollbarSlider-background) !important; }.scrollbarSlider-hoverBackground { background-color:  var(--vscode-scrollbarSlider-hoverBackground) !important; }.scrollbarSlider-activeBackground { background-color:  var(--vscode-scrollbarSlider-activeBackground) !important; }.progressBar-background { background-color:  var(--vscode-progressBar-background) !important; }.editorError-background { background-color:  var(--vscode-editorError-background) !important; }.editorError-foreground { color: var(--vscode-editorError-foreground) !important; }.editorError-border { border-color:  var(--vscode-editorError-border) !important; }.editorWarning-background { background-color:  var(--vscode-editorWarning-background) !important; }.editorWarning-foreground { color: var(--vscode-editorWarning-foreground) !important; }.editorWarning-border { border-color:  var(--vscode-editorWarning-border) !important; }.editorInfo-background { background-color:  var(--vscode-editorInfo-background) !important; }.editorInfo-foreground { color: var(--vscode-editorInfo-foreground) !important; }.editorInfo-border { border-color:  var(--vscode-editorInfo-border) !important; }.editorHint-foreground { color: var(--vscode-editorHint-foreground) !important; }.editorHint-border { border-color:  var(--vscode-editorHint-border) !important; }.sash-hoverBorder { border-color:  var(--vscode-sash-hoverBorder) !important; }.editor-background { background-color:  var(--vscode-editor-background) !important; }.editor-foreground { color: var(--vscode-editor-foreground) !important; }.editorStickyScroll-background { background-color:  var(--vscode-editorStickyScroll-background) !important; }.editorStickyScrollHover-background { background-color:  var(--vscode-editorStickyScrollHover-background) !important; }.editorWidget-background { background-color:  var(--vscode-editorWidget-background) !important; }.editorWidget-foreground { color: var(--vscode-editorWidget-foreground) !important; }.editorWidget-border { border-color:  var(--vscode-editorWidget-border) !important; }.editorWidget-resizeBorder { border-color:  var(--vscode-editorWidget-resizeBorder) !important; }.quickInput-background { background-color:  var(--vscode-quickInput-background) !important; }.quickInput-foreground { color: var(--vscode-quickInput-foreground) !important; }.quickInputTitle-background { background-color:  var(--vscode-quickInputTitle-background) !important; }.pickerGroup-foreground { color: var(--vscode-pickerGroup-foreground) !important; }.pickerGroup-border { border-color:  var(--vscode-pickerGroup-border) !important; }.keybindingLabel-background { background-color:  var(--vscode-keybindingLabel-background) !important; }.keybindingLabel-foreground { color: var(--vscode-keybindingLabel-foreground) !important; }.keybindingLabel-border { border-color:  var(--vscode-keybindingLabel-border) !important; }.keybindingLabel-bottomBorder { border-color:  var(--vscode-keybindingLabel-bottomBorder) !important; }.editor-selectionBackground { background-color:  var(--vscode-editor-selectionBackground) !important; }.editor-selectionForeground { color: var(--vscode-editor-selectionForeground) !important; }.editor-inactiveSelectionBackground { background-color:  var(--vscode-editor-inactiveSelectionBackground) !important; }.editor-selectionHighlightBackground { background-color:  var(--vscode-editor-selectionHighlightBackground) !important; }.editor-selectionHighlightBorder { border-color:  var(--vscode-editor-selectionHighlightBorder) !important; }.editor-findMatchBackground { background-color:  var(--vscode-editor-findMatchBackground) !important; }.editor-findMatchHighlightBackground { background-color:  var(--vscode-editor-findMatchHighlightBackground) !important; }.editor-findRangeHighlightBackground { background-color:  var(--vscode-editor-findRangeHighlightBackground) !important; }.editor-findMatchBorder { border-color:  var(--vscode-editor-findMatchBorder) !important; }.editor-findMatchHighlightBorder { border-color:  var(--vscode-editor-findMatchHighlightBorder) !important; }.editor-findRangeHighlightBorder { border-color:  var(--vscode-editor-findRangeHighlightBorder) !important; }.searchEditor-findMatchBackground { background-color:  var(--vscode-searchEditor-findMatchBackground) !important; }.searchEditor-findMatchBorder { border-color:  var(--vscode-searchEditor-findMatchBorder) !important; }.editor-hoverHighlightBackground { background-color:  var(--vscode-editor-hoverHighlightBackground) !important; }.editorHoverWidget-background { background-color:  var(--vscode-editorHoverWidget-background) !important; }.editorHoverWidget-foreground { color: var(--vscode-editorHoverWidget-foreground) !important; }.editorHoverWidget-border { border-color:  var(--vscode-editorHoverWidget-border) !important; }.editorHoverWidget-statusBarBackground { background-color:  var(--vscode-editorHoverWidget-statusBarBackground) !important; }.editorLink-activeForeground { color: var(--vscode-editorLink-activeForeground) !important; }.editorInlayHint-foreground { color: var(--vscode-editorInlayHint-foreground) !important; }.editorInlayHint-background { background-color:  var(--vscode-editorInlayHint-background) !important; }.editorInlayHint-typeForeground { color: var(--vscode-editorInlayHint-typeForeground) !important; }.editorInlayHint-typeBackground { background-color:  var(--vscode-editorInlayHint-typeBackground) !important; }.editorInlayHint-parameterForeground { color: var(--vscode-editorInlayHint-parameterForeground) !important; }.editorInlayHint-parameterBackground { background-color:  var(--vscode-editorInlayHint-parameterBackground) !important; }.editorLightBulb-foreground { color: var(--vscode-editorLightBulb-foreground) !important; }.editorLightBulbAutoFix-foreground { color: var(--vscode-editorLightBulbAutoFix-foreground) !important; }.diffEditor-insertedTextBackground { background-color:  var(--vscode-diffEditor-insertedTextBackground) !important; }.diffEditor-removedTextBackground { background-color:  var(--vscode-diffEditor-removedTextBackground) !important; }.diffEditor-insertedLineBackground { background-color:  var(--vscode-diffEditor-insertedLineBackground) !important; }.diffEditor-removedLineBackground { background-color:  var(--vscode-diffEditor-removedLineBackground) !important; }.diffEditorGutter-insertedLineBackground { background-color:  var(--vscode-diffEditorGutter-insertedLineBackground) !important; }.diffEditorGutter-removedLineBackground { background-color:  var(--vscode-diffEditorGutter-removedLineBackground) !important; }.diffEditorOverview-insertedForeground { color: var(--vscode-diffEditorOverview-insertedForeground) !important; }.diffEditorOverview-removedForeground { color: var(--vscode-diffEditorOverview-removedForeground) !important; }.diffEditor-insertedTextBorder { border-color:  var(--vscode-diffEditor-insertedTextBorder) !important; }.diffEditor-removedTextBorder { border-color:  var(--vscode-diffEditor-removedTextBorder) !important; }.diffEditor-border { border-color:  var(--vscode-diffEditor-border) !important; }.list-focusBackground { background-color:  var(--vscode-list-focusBackground) !important; }.list-focusForeground { color: var(--vscode-list-focusForeground) !important; }.list-activeSelectionBackground { background-color:  var(--vscode-list-activeSelectionBackground) !important; }.list-activeSelectionForeground { color: var(--vscode-list-activeSelectionForeground) !important; }.list-activeSelectionIconForeground { color: var(--vscode-list-activeSelectionIconForeground) !important; }.list-inactiveSelectionBackground { background-color:  var(--vscode-list-inactiveSelectionBackground) !important; }.list-inactiveSelectionForeground { color: var(--vscode-list-inactiveSelectionForeground) !important; }.list-inactiveSelectionIconForeground { color: var(--vscode-list-inactiveSelectionIconForeground) !important; }.list-inactiveFocusBackground { background-color:  var(--vscode-list-inactiveFocusBackground) !important; }.list-hoverBackground { background-color:  var(--vscode-list-hoverBackground) !important; }.list-hoverForeground { color: var(--vscode-list-hoverForeground) !important; }.list-dropBackground { background-color:  var(--vscode-list-dropBackground) !important; }.list-highlightForeground { color: var(--vscode-list-highlightForeground) !important; }.list-focusHighlightForeground { color: var(--vscode-list-focusHighlightForeground) !important; }.list-invalidItemForeground { color: var(--vscode-list-invalidItemForeground) !important; }.list-errorForeground { color: var(--vscode-list-errorForeground) !important; }.list-warningForeground { color: var(--vscode-list-warningForeground) !important; }.listFilterWidget-background { background-color:  var(--vscode-listFilterWidget-background) !important; }.list-filterMatchBackground { background-color:  var(--vscode-list-filterMatchBackground) !important; }.list-filterMatchBorder { border-color:  var(--vscode-list-filterMatchBorder) !important; }.tree-tableColumnsBorder { border-color:  var(--vscode-tree-tableColumnsBorder) !important; }.tree-tableOddRowsBackground { background-color:  var(--vscode-tree-tableOddRowsBackground) !important; }.list-deemphasizedForeground { color: var(--vscode-list-deemphasizedForeground) !important; }.checkbox-background { background-color:  var(--vscode-checkbox-background) !important; }.checkbox-selectBackground { background-color:  var(--vscode-checkbox-selectBackground) !important; }.checkbox-foreground { color: var(--vscode-checkbox-foreground) !important; }.checkbox-border { border-color:  var(--vscode-checkbox-border) !important; }.checkbox-selectBorder { border-color:  var(--vscode-checkbox-selectBorder) !important; }.quickInput-list-focusBackground { background-color:  var(--vscode-quickInput-list-focusBackground) !important; }.quickInputList-focusForeground { color: var(--vscode-quickInputList-focusForeground) !important; }.quickInputList-focusIconForeground { color: var(--vscode-quickInputList-focusIconForeground) !important; }.quickInputList-focusBackground { background-color:  var(--vscode-quickInputList-focusBackground) !important; }.menu-border { border-color:  var(--vscode-menu-border) !important; }.menu-foreground { color: var(--vscode-menu-foreground) !important; }.menu-background { background-color:  var(--vscode-menu-background) !important; }.menu-selectionForeground { color: var(--vscode-menu-selectionForeground) !important; }.menu-selectionBackground { background-color:  var(--vscode-menu-selectionBackground) !important; }.menu-selectionBorder { border-color:  var(--vscode-menu-selectionBorder) !important; }.menu-separatorBackground { background-color:  var(--vscode-menu-separatorBackground) !important; }.toolbar-hoverBackground { background-color:  var(--vscode-toolbar-hoverBackground) !important; }.toolbar-activeBackground { background-color:  var(--vscode-toolbar-activeBackground) !important; }.editor-snippetTabstopHighlightBackground { background-color:  var(--vscode-editor-snippetTabstopHighlightBackground) !important; }.editor-snippetTabstopHighlightBorder { border-color:  var(--vscode-editor-snippetTabstopHighlightBorder) !important; }.editor-snippetFinalTabstopHighlightBackground { background-color:  var(--vscode-editor-snippetFinalTabstopHighlightBackground) !important; }.editor-snippetFinalTabstopHighlightBorder { border-color:  var(--vscode-editor-snippetFinalTabstopHighlightBorder) !important; }.breadcrumb-foreground { color: var(--vscode-breadcrumb-foreground) !important; }.breadcrumb-background { background-color:  var(--vscode-breadcrumb-background) !important; }.breadcrumb-focusForeground { color: var(--vscode-breadcrumb-focusForeground) !important; }.breadcrumb-activeSelectionForeground { color: var(--vscode-breadcrumb-activeSelectionForeground) !important; }.breadcrumbPicker-background { background-color:  var(--vscode-breadcrumbPicker-background) !important; }.merge-currentHeaderBackground { background-color:  var(--vscode-merge-currentHeaderBackground) !important; }.merge-currentContentBackground { background-color:  var(--vscode-merge-currentContentBackground) !important; }.merge-incomingHeaderBackground { background-color:  var(--vscode-merge-incomingHeaderBackground) !important; }.merge-incomingContentBackground { background-color:  var(--vscode-merge-incomingContentBackground) !important; }.merge-commonHeaderBackground { background-color:  var(--vscode-merge-commonHeaderBackground) !important; }.merge-commonContentBackground { background-color:  var(--vscode-merge-commonContentBackground) !important; }.merge-border { border-color:  var(--vscode-merge-border) !important; }.editorOverviewRuler-currentContentForeground { color: var(--vscode-editorOverviewRuler-currentContentForeground) !important; }.editorOverviewRuler-incomingContentForeground { color: var(--vscode-editorOverviewRuler-incomingContentForeground) !important; }.editorOverviewRuler-commonContentForeground { color: var(--vscode-editorOverviewRuler-commonContentForeground) !important; }.editorOverviewRuler-findMatchForeground { color: var(--vscode-editorOverviewRuler-findMatchForeground) !important; }.editorOverviewRuler-selectionHighlightForeground { color: var(--vscode-editorOverviewRuler-selectionHighlightForeground) !important; }.minimap-background { background-color:  var(--vscode-minimap-background) !important; }.minimap-foregroundOpacity { color: var(--vscode-minimap-foregroundOpacity) !important; }.minimapSlider-background { background-color:  var(--vscode-minimapSlider-background) !important; }.minimapSlider-hoverBackground { background-color:  var(--vscode-minimapSlider-hoverBackground) !important; }.minimapSlider-activeBackground { background-color:  var(--vscode-minimapSlider-activeBackground) !important; }.problemsErrorIcon-foreground { color: var(--vscode-problemsErrorIcon-foreground) !important; }.problemsWarningIcon-foreground { color: var(--vscode-problemsWarningIcon-foreground) !important; }.problemsInfoIcon-foreground { color: var(--vscode-problemsInfoIcon-foreground) !important; }.charts-foreground { color: var(--vscode-charts-foreground) !important; }.symbolIcon-arrayForeground { color: var(--vscode-symbolIcon-arrayForeground) !important; }.symbolIcon-booleanForeground { color: var(--vscode-symbolIcon-booleanForeground) !important; }.symbolIcon-classForeground { color: var(--vscode-symbolIcon-classForeground) !important; }.symbolIcon-colorForeground { color: var(--vscode-symbolIcon-colorForeground) !important; }.symbolIcon-constantForeground { color: var(--vscode-symbolIcon-constantForeground) !important; }.symbolIcon-constructorForeground { color: var(--vscode-symbolIcon-constructorForeground) !important; }.symbolIcon-enumeratorForeground { color: var(--vscode-symbolIcon-enumeratorForeground) !important; }.symbolIcon-enumeratorMemberForeground { color: var(--vscode-symbolIcon-enumeratorMemberForeground) !important; }.symbolIcon-eventForeground { color: var(--vscode-symbolIcon-eventForeground) !important; }.symbolIcon-fieldForeground { color: var(--vscode-symbolIcon-fieldForeground) !important; }.symbolIcon-fileForeground { color: var(--vscode-symbolIcon-fileForeground) !important; }.symbolIcon-folderForeground { color: var(--vscode-symbolIcon-folderForeground) !important; }.symbolIcon-functionForeground { color: var(--vscode-symbolIcon-functionForeground) !important; }.symbolIcon-interfaceForeground { color: var(--vscode-symbolIcon-interfaceForeground) !important; }.symbolIcon-keyForeground { color: var(--vscode-symbolIcon-keyForeground) !important; }.symbolIcon-keywordForeground { color: var(--vscode-symbolIcon-keywordForeground) !important; }.symbolIcon-methodForeground { color: var(--vscode-symbolIcon-methodForeground) !important; }.symbolIcon-moduleForeground { color: var(--vscode-symbolIcon-moduleForeground) !important; }.symbolIcon-namespaceForeground { color: var(--vscode-symbolIcon-namespaceForeground) !important; }.symbolIcon-nullForeground { color: var(--vscode-symbolIcon-nullForeground) !important; }.symbolIcon-numberForeground { color: var(--vscode-symbolIcon-numberForeground) !important; }.symbolIcon-objectForeground { color: var(--vscode-symbolIcon-objectForeground) !important; }.symbolIcon-operatorForeground { color: var(--vscode-symbolIcon-operatorForeground) !important; }.symbolIcon-packageForeground { color: var(--vscode-symbolIcon-packageForeground) !important; }.symbolIcon-propertyForeground { color: var(--vscode-symbolIcon-propertyForeground) !important; }.symbolIcon-referenceForeground { color: var(--vscode-symbolIcon-referenceForeground) !important; }.symbolIcon-snippetForeground { color: var(--vscode-symbolIcon-snippetForeground) !important; }.symbolIcon-stringForeground { color: var(--vscode-symbolIcon-stringForeground) !important; }.symbolIcon-structForeground { color: var(--vscode-symbolIcon-structForeground) !important; }.symbolIcon-textForeground { color: var(--vscode-symbolIcon-textForeground) !important; }.symbolIcon-typeParameterForeground { color: var(--vscode-symbolIcon-typeParameterForeground) !important; }.symbolIcon-unitForeground { color: var(--vscode-symbolIcon-unitForeground) !important; }.symbolIcon-variableForeground { color: var(--vscode-symbolIcon-variableForeground) !important; }.editor-lineHighlightBackground { background-color:  var(--vscode-editor-lineHighlightBackground) !important; }.editor-lineHighlightBorder { border-color:  var(--vscode-editor-lineHighlightBorder) !important; }.editor-rangeHighlightBackground { background-color:  var(--vscode-editor-rangeHighlightBackground) !important; }.editor-rangeHighlightBorder { border-color:  var(--vscode-editor-rangeHighlightBorder) !important; }.editor-symbolHighlightBackground { background-color:  var(--vscode-editor-symbolHighlightBackground) !important; }.editor-symbolHighlightBorder { border-color:  var(--vscode-editor-symbolHighlightBorder) !important; }.editorCursor-foreground { color: var(--vscode-editorCursor-foreground) !important; }.editorCursor-background { background-color:  var(--vscode-editorCursor-background) !important; }.editorWhitespace-foreground { color: var(--vscode-editorWhitespace-foreground) !important; }.editorIndentGuide-background { background-color:  var(--vscode-editorIndentGuide-background) !important; }.editorIndentGuide-activeBackground { background-color:  var(--vscode-editorIndentGuide-activeBackground) !important; }.editorLineNumber-foreground { color: var(--vscode-editorLineNumber-foreground) !important; }.editorActiveLineNumber-foreground { color: var(--vscode-editorActiveLineNumber-foreground) !important; }.editorLineNumber-activeForeground { color: var(--vscode-editorLineNumber-activeForeground) !important; }.editorRuler-foreground { color: var(--vscode-editorRuler-foreground) !important; }.editorCodeLens-foreground { color: var(--vscode-editorCodeLens-foreground) !important; }.editorBracketMatch-background { background-color:  var(--vscode-editorBracketMatch-background) !important; }.editorBracketMatch-border { border-color:  var(--vscode-editorBracketMatch-border) !important; }.editorOverviewRuler-border { border-color:  var(--vscode-editorOverviewRuler-border) !important; }.editorOverviewRuler-background { background-color:  var(--vscode-editorOverviewRuler-background) !important; }.editorGutter-background { background-color:  var(--vscode-editorGutter-background) !important; }.editorUnnecessaryCode-border { border-color:  var(--vscode-editorUnnecessaryCode-border) !important; }.editorGhostText-border { border-color:  var(--vscode-editorGhostText-border) !important; }.editorGhostText-foreground { color: var(--vscode-editorGhostText-foreground) !important; }.editorGhostText-background { background-color:  var(--vscode-editorGhostText-background) !important; }.editorOverviewRuler-rangeHighlightForeground { color: var(--vscode-editorOverviewRuler-rangeHighlightForeground) !important; }.editorOverviewRuler-errorForeground { color: var(--vscode-editorOverviewRuler-errorForeground) !important; }.editorOverviewRuler-warningForeground { color: var(--vscode-editorOverviewRuler-warningForeground) !important; }.editorOverviewRuler-infoForeground { color: var(--vscode-editorOverviewRuler-infoForeground) !important; }.editorBracketHighlight-foreground1 { color: var(--vscode-editorBracketHighlight-foreground1) !important; }.editorBracketHighlight-foreground2 { color: var(--vscode-editorBracketHighlight-foreground2) !important; }.editorBracketHighlight-foreground3 { color: var(--vscode-editorBracketHighlight-foreground3) !important; }.editorBracketHighlight-foreground4 { color: var(--vscode-editorBracketHighlight-foreground4) !important; }.editorBracketHighlight-foreground5 { color: var(--vscode-editorBracketHighlight-foreground5) !important; }.editorBracketHighlight-foreground6 { color: var(--vscode-editorBracketHighlight-foreground6) !important; }.editorBracketHighlight-unexpectedBracket-foreground { color: var(--vscode-editorBracketHighlight-unexpectedBracket-foreground) !important; }.editorBracketPairGuide-background1 { background-color:  var(--vscode-editorBracketPairGuide-background1) !important; }.editorBracketPairGuide-background2 { background-color:  var(--vscode-editorBracketPairGuide-background2) !important; }.editorBracketPairGuide-background3 { background-color:  var(--vscode-editorBracketPairGuide-background3) !important; }.editorBracketPairGuide-background4 { background-color:  var(--vscode-editorBracketPairGuide-background4) !important; }.editorBracketPairGuide-background5 { background-color:  var(--vscode-editorBracketPairGuide-background5) !important; }.editorBracketPairGuide-background6 { background-color:  var(--vscode-editorBracketPairGuide-background6) !important; }.editorBracketPairGuide-activeBackground1 { background-color:  var(--vscode-editorBracketPairGuide-activeBackground1) !important; }.editorBracketPairGuide-activeBackground2 { background-color:  var(--vscode-editorBracketPairGuide-activeBackground2) !important; }.editorBracketPairGuide-activeBackground3 { background-color:  var(--vscode-editorBracketPairGuide-activeBackground3) !important; }.editorBracketPairGuide-activeBackground4 { background-color:  var(--vscode-editorBracketPairGuide-activeBackground4) !important; }.editorBracketPairGuide-activeBackground5 { background-color:  var(--vscode-editorBracketPairGuide-activeBackground5) !important; }.editorBracketPairGuide-activeBackground6 { background-color:  var(--vscode-editorBracketPairGuide-activeBackground6) !important; }.editorUnicodeHighlight-border { border-color:  var(--vscode-editorUnicodeHighlight-border) !important; }.editorUnicodeHighlight-background { background-color:  var(--vscode-editorUnicodeHighlight-background) !important; }.editorHoverWidget-highlightForeground { color: var(--vscode-editorHoverWidget-highlightForeground) !important; }.editorOverviewRuler-bracketMatchForeground { color: var(--vscode-editorOverviewRuler-bracketMatchForeground) !important; }.editor-foldBackground { background-color:  var(--vscode-editor-foldBackground) !important; }.editorGutter-foldingControlForeground { color: var(--vscode-editorGutter-foldingControlForeground) !important; }.editor-linkedEditingBackground { background-color:  var(--vscode-editor-linkedEditingBackground) !important; }.editor-wordHighlightBackground { background-color:  var(--vscode-editor-wordHighlightBackground) !important; }.editor-wordHighlightStrongBackground { background-color:  var(--vscode-editor-wordHighlightStrongBackground) !important; }.editor-wordHighlightBorder { border-color:  var(--vscode-editor-wordHighlightBorder) !important; }.editor-wordHighlightStrongBorder { border-color:  var(--vscode-editor-wordHighlightStrongBorder) !important; }.editorOverviewRuler-wordHighlightForeground { color: var(--vscode-editorOverviewRuler-wordHighlightForeground) !important; }.editorOverviewRuler-wordHighlightStrongForeground { color: var(--vscode-editorOverviewRuler-wordHighlightStrongForeground) !important; }.peekViewTitle-background { background-color:  var(--vscode-peekViewTitle-background) !important; }.peekViewTitleLabel-foreground { color: var(--vscode-peekViewTitleLabel-foreground) !important; }.peekViewTitleDescription-foreground { color: var(--vscode-peekViewTitleDescription-foreground) !important; }.peekView-border { border-color:  var(--vscode-peekView-border) !important; }.peekViewResult-background { background-color:  var(--vscode-peekViewResult-background) !important; }.peekViewResult-lineForeground { color: var(--vscode-peekViewResult-lineForeground) !important; }.peekViewResult-fileForeground { color: var(--vscode-peekViewResult-fileForeground) !important; }.peekViewResult-selectionBackground { background-color:  var(--vscode-peekViewResult-selectionBackground) !important; }.peekViewResult-selectionForeground { color: var(--vscode-peekViewResult-selectionForeground) !important; }.peekViewEditor-background { background-color:  var(--vscode-peekViewEditor-background) !important; }.peekViewEditorGutter-background { background-color:  var(--vscode-peekViewEditorGutter-background) !important; }.peekViewResult-matchHighlightBackground { background-color:  var(--vscode-peekViewResult-matchHighlightBackground) !important; }.peekViewEditor-matchHighlightBackground { background-color:  var(--vscode-peekViewEditor-matchHighlightBackground) !important; }.peekViewEditor-matchHighlightBorder { border-color:  var(--vscode-peekViewEditor-matchHighlightBorder) !important; }.editorMarkerNavigationError-background { background-color:  var(--vscode-editorMarkerNavigationError-background) !important; }.editorMarkerNavigationError-headerBackground { background-color:  var(--vscode-editorMarkerNavigationError-headerBackground) !important; }.editorMarkerNavigationWarning-background { background-color:  var(--vscode-editorMarkerNavigationWarning-background) !important; }.editorMarkerNavigationWarning-headerBackground { background-color:  var(--vscode-editorMarkerNavigationWarning-headerBackground) !important; }.editorMarkerNavigationInfo-background { background-color:  var(--vscode-editorMarkerNavigationInfo-background) !important; }.editorMarkerNavigationInfo-headerBackground { background-color:  var(--vscode-editorMarkerNavigationInfo-headerBackground) !important; }.editorMarkerNavigation-background { background-color:  var(--vscode-editorMarkerNavigation-background) !important; }.editorSuggestWidget-background { background-color:  var(--vscode-editorSuggestWidget-background) !important; }.editorSuggestWidget-border { border-color:  var(--vscode-editorSuggestWidget-border) !important; }.editorSuggestWidget-foreground { color: var(--vscode-editorSuggestWidget-foreground) !important; }.editorSuggestWidget-selectedForeground { color: var(--vscode-editorSuggestWidget-selectedForeground) !important; }.editorSuggestWidget-selectedIconForeground { color: var(--vscode-editorSuggestWidget-selectedIconForeground) !important; }.editorSuggestWidget-selectedBackground { background-color:  var(--vscode-editorSuggestWidget-selectedBackground) !important; }.editorSuggestWidget-highlightForeground { color: var(--vscode-editorSuggestWidget-highlightForeground) !important; }.editorSuggestWidget-focusHighlightForeground { color: var(--vscode-editorSuggestWidget-focusHighlightForeground) !important; }.editorSuggestWidgetStatus-foreground { color: var(--vscode-editorSuggestWidgetStatus-foreground) !important; }.tab-activeBackground { background-color:  var(--vscode-tab-activeBackground) !important; }.tab-unfocusedActiveBackground { background-color:  var(--vscode-tab-unfocusedActiveBackground) !important; }.tab-inactiveBackground { background-color:  var(--vscode-tab-inactiveBackground) !important; }.tab-unfocusedInactiveBackground { background-color:  var(--vscode-tab-unfocusedInactiveBackground) !important; }.tab-activeForeground { color: var(--vscode-tab-activeForeground) !important; }.tab-inactiveForeground { color: var(--vscode-tab-inactiveForeground) !important; }.tab-unfocusedActiveForeground { color: var(--vscode-tab-unfocusedActiveForeground) !important; }.tab-unfocusedInactiveForeground { color: var(--vscode-tab-unfocusedInactiveForeground) !important; }.tab-hoverBackground { background-color:  var(--vscode-tab-hoverBackground) !important; }.tab-unfocusedHoverBackground { background-color:  var(--vscode-tab-unfocusedHoverBackground) !important; }.tab-hoverForeground { color: var(--vscode-tab-hoverForeground) !important; }.tab-unfocusedHoverForeground { color: var(--vscode-tab-unfocusedHoverForeground) !important; }.tab-border { border-color:  var(--vscode-tab-border) !important; }.tab-lastPinnedBorder { border-color:  var(--vscode-tab-lastPinnedBorder) !important; }.tab-activeBorder { border-color:  var(--vscode-tab-activeBorder) !important; }.tab-unfocusedActiveBorder { border-color:  var(--vscode-tab-unfocusedActiveBorder) !important; }.tab-activeBorderTop { border-color:  var(--vscode-tab-activeBorderTop) !important; }.tab-unfocusedActiveBorderTop { border-color:  var(--vscode-tab-unfocusedActiveBorderTop) !important; }.tab-hoverBorder { border-color:  var(--vscode-tab-hoverBorder) !important; }.tab-unfocusedHoverBorder { border-color:  var(--vscode-tab-unfocusedHoverBorder) !important; }.tab-activeModifiedBorder { border-color:  var(--vscode-tab-activeModifiedBorder) !important; }.tab-inactiveModifiedBorder { border-color:  var(--vscode-tab-inactiveModifiedBorder) !important; }.tab-unfocusedActiveModifiedBorder { border-color:  var(--vscode-tab-unfocusedActiveModifiedBorder) !important; }.tab-unfocusedInactiveModifiedBorder { border-color:  var(--vscode-tab-unfocusedInactiveModifiedBorder) !important; }.editorPane-background { background-color:  var(--vscode-editorPane-background) !important; }.editorGroup-emptyBackground { background-color:  var(--vscode-editorGroup-emptyBackground) !important; }.editorGroup-focusedEmptyBorder { border-color:  var(--vscode-editorGroup-focusedEmptyBorder) !important; }.editorGroupHeader-tabsBackground { background-color:  var(--vscode-editorGroupHeader-tabsBackground) !important; }.editorGroupHeader-tabsBorder { border-color:  var(--vscode-editorGroupHeader-tabsBorder) !important; }.editorGroupHeader-noTabsBackground { background-color:  var(--vscode-editorGroupHeader-noTabsBackground) !important; }.editorGroupHeader-border { border-color:  var(--vscode-editorGroupHeader-border) !important; }.editorGroup-border { border-color:  var(--vscode-editorGroup-border) !important; }.editorGroup-dropBackground { background-color:  var(--vscode-editorGroup-dropBackground) !important; }.editorGroup-dropIntoPromptForeground { color: var(--vscode-editorGroup-dropIntoPromptForeground) !important; }.editorGroup-dropIntoPromptBackground { background-color:  var(--vscode-editorGroup-dropIntoPromptBackground) !important; }.editorGroup-dropIntoPromptBorder { border-color:  var(--vscode-editorGroup-dropIntoPromptBorder) !important; }.sideBySideEditor-horizontalBorder { border-color:  var(--vscode-sideBySideEditor-horizontalBorder) !important; }.sideBySideEditor-verticalBorder { border-color:  var(--vscode-sideBySideEditor-verticalBorder) !important; }.panel-background { background-color:  var(--vscode-panel-background) !important; }.panel-border { border-color:  var(--vscode-panel-border) !important; }.panelTitle-activeForeground { color: var(--vscode-panelTitle-activeForeground) !important; }.panelTitle-inactiveForeground { color: var(--vscode-panelTitle-inactiveForeground) !important; }.panelTitle-activeBorder { border-color:  var(--vscode-panelTitle-activeBorder) !important; }.panelInput-border { border-color:  var(--vscode-panelInput-border) !important; }.panel-dropBorder { border-color:  var(--vscode-panel-dropBorder) !important; }.panelSection-dropBackground { background-color:  var(--vscode-panelSection-dropBackground) !important; }.panelSectionHeader-background { background-color:  var(--vscode-panelSectionHeader-background) !important; }.panelSectionHeader-foreground { color: var(--vscode-panelSectionHeader-foreground) !important; }.panelSectionHeader-border { border-color:  var(--vscode-panelSectionHeader-border) !important; }.panelSection-border { border-color:  var(--vscode-panelSection-border) !important; }.banner-background { background-color:  var(--vscode-banner-background) !important; }.banner-foreground { color: var(--vscode-banner-foreground) !important; }.banner-iconForeground { color: var(--vscode-banner-iconForeground) !important; }.statusBar-foreground { color: var(--vscode-statusBar-foreground) !important; }.statusBar-noFolderForeground { color: var(--vscode-statusBar-noFolderForeground) !important; }.statusBar-background { background-color:  var(--vscode-statusBar-background) !important; }.statusBar-noFolderBackground { background-color:  var(--vscode-statusBar-noFolderBackground) !important; }.statusBar-border { border-color:  var(--vscode-statusBar-border) !important; }.statusBar-focusBorder { border-color:  var(--vscode-statusBar-focusBorder) !important; }.statusBar-noFolderBorder { border-color:  var(--vscode-statusBar-noFolderBorder) !important; }.statusBarItem-activeBackground { background-color:  var(--vscode-statusBarItem-activeBackground) !important; }.statusBarItem-focusBorder { border-color:  var(--vscode-statusBarItem-focusBorder) !important; }.statusBarItem-hoverBackground { background-color:  var(--vscode-statusBarItem-hoverBackground) !important; }.statusBarItem-compactHoverBackground { background-color:  var(--vscode-statusBarItem-compactHoverBackground) !important; }.statusBarItem-prominentForeground { color: var(--vscode-statusBarItem-prominentForeground) !important; }.statusBarItem-prominentBackground { background-color:  var(--vscode-statusBarItem-prominentBackground) !important; }.statusBarItem-prominentHoverBackground { background-color:  var(--vscode-statusBarItem-prominentHoverBackground) !important; }.statusBarItem-errorBackground { background-color:  var(--vscode-statusBarItem-errorBackground) !important; }.statusBarItem-errorForeground { color: var(--vscode-statusBarItem-errorForeground) !important; }.statusBarItem-warningBackground { background-color:  var(--vscode-statusBarItem-warningBackground) !important; }.statusBarItem-warningForeground { color: var(--vscode-statusBarItem-warningForeground) !important; }.activityBar-background { background-color:  var(--vscode-activityBar-background) !important; }.activityBar-foreground { color: var(--vscode-activityBar-foreground) !important; }.activityBar-inactiveForeground { color: var(--vscode-activityBar-inactiveForeground) !important; }.activityBar-border { border-color:  var(--vscode-activityBar-border) !important; }.activityBar-activeBorder { border-color:  var(--vscode-activityBar-activeBorder) !important; }.activityBar-activeFocusBorder { border-color:  var(--vscode-activityBar-activeFocusBorder) !important; }.activityBar-activeBackground { background-color:  var(--vscode-activityBar-activeBackground) !important; }.activityBar-dropBorder { border-color:  var(--vscode-activityBar-dropBorder) !important; }.activityBarBadge-background { background-color:  var(--vscode-activityBarBadge-background) !important; }.activityBarBadge-foreground { color: var(--vscode-activityBarBadge-foreground) !important; }.activityBarItem-profilesForeground { color: var(--vscode-activityBarItem-profilesForeground) !important; }.activityBarItem-profilesHoverForeground { color: var(--vscode-activityBarItem-profilesHoverForeground) !important; }.activityBarItem-profilesBackground { background-color:  var(--vscode-activityBarItem-profilesBackground) !important; }.statusBarItem-remoteBackground { background-color:  var(--vscode-statusBarItem-remoteBackground) !important; }.statusBarItem-remoteForeground { color: var(--vscode-statusBarItem-remoteForeground) !important; }.extensionBadge-remoteBackground { background-color:  var(--vscode-extensionBadge-remoteBackground) !important; }.extensionBadge-remoteForeground { color: var(--vscode-extensionBadge-remoteForeground) !important; }.sideBar-background { background-color:  var(--vscode-sideBar-background) !important; }.sideBar-foreground { color: var(--vscode-sideBar-foreground) !important; }.sideBar-border { border-color:  var(--vscode-sideBar-border) !important; }.sideBarTitle-foreground { color: var(--vscode-sideBarTitle-foreground) !important; }.sideBar-dropBackground { background-color:  var(--vscode-sideBar-dropBackground) !important; }.sideBarSectionHeader-background { background-color:  var(--vscode-sideBarSectionHeader-background) !important; }.sideBarSectionHeader-foreground { color: var(--vscode-sideBarSectionHeader-foreground) !important; }.sideBarSectionHeader-border { border-color:  var(--vscode-sideBarSectionHeader-border) !important; }.titleBar-activeForeground { color: var(--vscode-titleBar-activeForeground) !important; }.titleBar-inactiveForeground { color: var(--vscode-titleBar-inactiveForeground) !important; }.titleBar-activeBackground { background-color:  var(--vscode-titleBar-activeBackground) !important; }.titleBar-inactiveBackground { background-color:  var(--vscode-titleBar-inactiveBackground) !important; }.titleBar-border { border-color:  var(--vscode-titleBar-border) !important; }.menubar-selectionForeground { color: var(--vscode-menubar-selectionForeground) !important; }.menubar-selectionBackground { background-color:  var(--vscode-menubar-selectionBackground) !important; }.menubar-selectionBorder { border-color:  var(--vscode-menubar-selectionBorder) !important; }.notificationCenter-border { border-color:  var(--vscode-notificationCenter-border) !important; }.notificationToast-border { border-color:  var(--vscode-notificationToast-border) !important; }.notifications-foreground { color: var(--vscode-notifications-foreground) !important; }.notifications-background { background-color:  var(--vscode-notifications-background) !important; }.notificationLink-foreground { color: var(--vscode-notificationLink-foreground) !important; }.notificationCenterHeader-foreground { color: var(--vscode-notificationCenterHeader-foreground) !important; }.notificationCenterHeader-background { background-color:  var(--vscode-notificationCenterHeader-background) !important; }.notifications-border { border-color:  var(--vscode-notifications-border) !important; }.notificationsErrorIcon-foreground { color: var(--vscode-notificationsErrorIcon-foreground) !important; }.notificationsWarningIcon-foreground { color: var(--vscode-notificationsWarningIcon-foreground) !important; }.notificationsInfoIcon-foreground { color: var(--vscode-notificationsInfoIcon-foreground) !important; }.window-activeBorder { border-color:  var(--vscode-window-activeBorder) !important; }.window-inactiveBorder { border-color:  var(--vscode-window-inactiveBorder) !important; }.commandCenter-foreground { color: var(--vscode-commandCenter-foreground) !important; }.commandCenter-activeForeground { color: var(--vscode-commandCenter-activeForeground) !important; }.commandCenter-inactiveForeground { color: var(--vscode-commandCenter-inactiveForeground) !important; }.commandCenter-background { background-color:  var(--vscode-commandCenter-background) !important; }.commandCenter-activeBackground { background-color:  var(--vscode-commandCenter-activeBackground) !important; }.commandCenter-border { border-color:  var(--vscode-commandCenter-border) !important; }.commandCenter-activeBorder { border-color:  var(--vscode-commandCenter-activeBorder) !important; }.commandCenter-inactiveBorder { border-color:  var(--vscode-commandCenter-inactiveBorder) !important; }.editorCommentsWidget-resolvedBorder { border-color:  var(--vscode-editorCommentsWidget-resolvedBorder) !important; }.editorCommentsWidget-unresolvedBorder { border-color:  var(--vscode-editorCommentsWidget-unresolvedBorder) !important; }.editorCommentsWidget-rangeBackground { background-color:  var(--vscode-editorCommentsWidget-rangeBackground) !important; }.editorCommentsWidget-rangeBorder { border-color:  var(--vscode-editorCommentsWidget-rangeBorder) !important; }.editorCommentsWidget-rangeActiveBackground { background-color:  var(--vscode-editorCommentsWidget-rangeActiveBackground) !important; }.editorCommentsWidget-rangeActiveBorder { border-color:  var(--vscode-editorCommentsWidget-rangeActiveBorder) !important; }.editorGutter-commentRangeForeground { color: var(--vscode-editorGutter-commentRangeForeground) !important; }.debugToolBar-background { background-color:  var(--vscode-debugToolBar-background) !important; }.debugToolBar-border { border-color:  var(--vscode-debugToolBar-border) !important; }.debugIcon-startForeground { color: var(--vscode-debugIcon-startForeground) !important; }.editor-stackFrameHighlightBackground { background-color:  var(--vscode-editor-stackFrameHighlightBackground) !important; }.editor-focusedStackFrameHighlightBackground { background-color:  var(--vscode-editor-focusedStackFrameHighlightBackground) !important; }.mergeEditor-change-background { background-color:  var(--vscode-mergeEditor-change-background) !important; }.mergeEditor-change-word-background { background-color:  var(--vscode-mergeEditor-change-word-background) !important; }.mergeEditor-changeBase-background { background-color:  var(--vscode-mergeEditor-changeBase-background) !important; }.mergeEditor-changeBase-word-background { background-color:  var(--vscode-mergeEditor-changeBase-word-background) !important; }.mergeEditor-conflict-unhandledUnfocused-border { border-color:  var(--vscode-mergeEditor-conflict-unhandledUnfocused-border) !important; }.mergeEditor-conflict-unhandledFocused-border { border-color:  var(--vscode-mergeEditor-conflict-unhandledFocused-border) !important; }.mergeEditor-conflict-handledUnfocused-border { border-color:  var(--vscode-mergeEditor-conflict-handledUnfocused-border) !important; }.mergeEditor-conflict-handledFocused-border { border-color:  var(--vscode-mergeEditor-conflict-handledFocused-border) !important; }.mergeEditor-conflictingLines-background { background-color:  var(--vscode-mergeEditor-conflictingLines-background) !important; }.mergeEditor-conflict-input1-background { background-color:  var(--vscode-mergeEditor-conflict-input1-background) !important; }.mergeEditor-conflict-input2-background { background-color:  var(--vscode-mergeEditor-conflict-input2-background) !important; }.settings-headerForeground { color: var(--vscode-settings-headerForeground) !important; }.settings-headerBorder { border-color:  var(--vscode-settings-headerBorder) !important; }.settings-sashBorder { border-color:  var(--vscode-settings-sashBorder) !important; }.settings-dropdownBackground { background-color:  var(--vscode-settings-dropdownBackground) !important; }.settings-dropdownForeground { color: var(--vscode-settings-dropdownForeground) !important; }.settings-dropdownBorder { border-color:  var(--vscode-settings-dropdownBorder) !important; }.settings-dropdownListBorder { border-color:  var(--vscode-settings-dropdownListBorder) !important; }.settings-checkboxBackground { background-color:  var(--vscode-settings-checkboxBackground) !important; }.settings-checkboxForeground { color: var(--vscode-settings-checkboxForeground) !important; }.settings-checkboxBorder { border-color:  var(--vscode-settings-checkboxBorder) !important; }.settings-textInputBackground { background-color:  var(--vscode-settings-textInputBackground) !important; }.settings-textInputForeground { color: var(--vscode-settings-textInputForeground) !important; }.settings-textInputBorder { border-color:  var(--vscode-settings-textInputBorder) !important; }.settings-numberInputBackground { background-color:  var(--vscode-settings-numberInputBackground) !important; }.settings-numberInputForeground { color: var(--vscode-settings-numberInputForeground) !important; }.settings-numberInputBorder { border-color:  var(--vscode-settings-numberInputBorder) !important; }.settings-focusedRowBackground { background-color:  var(--vscode-settings-focusedRowBackground) !important; }.settings-rowHoverBackground { background-color:  var(--vscode-settings-rowHoverBackground) !important; }.settings-focusedRowBorder { border-color:  var(--vscode-settings-focusedRowBorder) !important; }.terminal-background { background-color:  var(--vscode-terminal-background) !important; }.terminal-foreground { color: var(--vscode-terminal-foreground) !important; }.terminalCursor-foreground { color: var(--vscode-terminalCursor-foreground) !important; }.terminalCursor-background { background-color:  var(--vscode-terminalCursor-background) !important; }.terminal-selectionBackground { background-color:  var(--vscode-terminal-selectionBackground) !important; }.terminal-inactiveSelectionBackground { background-color:  var(--vscode-terminal-inactiveSelectionBackground) !important; }.terminal-selectionForeground { color: var(--vscode-terminal-selectionForeground) !important; }.terminalCommandDecoration-defaultBackground { background-color:  var(--vscode-terminalCommandDecoration-defaultBackground) !important; }.terminalCommandDecoration-successBackground { background-color:  var(--vscode-terminalCommandDecoration-successBackground) !important; }.terminalCommandDecoration-errorBackground { background-color:  var(--vscode-terminalCommandDecoration-errorBackground) !important; }.terminalOverviewRuler-cursorForeground { color: var(--vscode-terminalOverviewRuler-cursorForeground) !important; }.terminal-border { border-color:  var(--vscode-terminal-border) !important; }.terminal-findMatchBackground { background-color:  var(--vscode-terminal-findMatchBackground) !important; }.terminal-findMatchBorder { border-color:  var(--vscode-terminal-findMatchBorder) !important; }.terminal-findMatchHighlightBackground { background-color:  var(--vscode-terminal-findMatchHighlightBackground) !important; }.terminal-findMatchHighlightBorder { border-color:  var(--vscode-terminal-findMatchHighlightBorder) !important; }.terminalOverviewRuler-findMatchForeground { color: var(--vscode-terminalOverviewRuler-findMatchForeground) !important; }.terminal-dropBackground { background-color:  var(--vscode-terminal-dropBackground) !important; }.terminal-tab-activeBorder { border-color:  var(--vscode-terminal-tab-activeBorder) !important; }.testing-peekBorder { border-color:  var(--vscode-testing-peekBorder) !important; }.testing-peekHeaderBackground { background-color:  var(--vscode-testing-peekHeaderBackground) !important; }.testing-message-error-decorationForeground { color: var(--vscode-testing-message-error-decorationForeground) !important; }.testing-message-error-lineBackground { background-color:  var(--vscode-testing-message-error-lineBackground) !important; }.testing-message-info-decorationForeground { color: var(--vscode-testing-message-info-decorationForeground) !important; }.testing-message-info-lineBackground { background-color:  var(--vscode-testing-message-info-lineBackground) !important; }.welcomePage-background { background-color:  var(--vscode-welcomePage-background) !important; }.welcomePage-tileBackground { background-color:  var(--vscode-welcomePage-tileBackground) !important; }.welcomePage-tileHoverBackground { background-color:  var(--vscode-welcomePage-tileHoverBackground) !important; }.welcomePage-tileBorder { border-color:  var(--vscode-welcomePage-tileBorder) !important; }.welcomePage-progress-background { background-color:  var(--vscode-welcomePage-progress-background) !important; }.welcomePage-progress-foreground { color: var(--vscode-welcomePage-progress-foreground) !important; }.walkthrough-stepTitle-foreground { color: var(--vscode-walkthrough-stepTitle-foreground) !important; }.walkThrough-embeddedEditorBackground { background-color:  var(--vscode-walkThrough-embeddedEditorBackground) !important; }.debugExceptionWidget-border { border-color:  var(--vscode-debugExceptionWidget-border) !important; }.debugExceptionWidget-background { background-color:  var(--vscode-debugExceptionWidget-background) !important; }.ports-iconRunningProcessForeground { color: var(--vscode-ports-iconRunningProcessForeground) !important; }.statusBar-debuggingBackground { background-color:  var(--vscode-statusBar-debuggingBackground) !important; }.statusBar-debuggingForeground { color: var(--vscode-statusBar-debuggingForeground) !important; }.statusBar-debuggingBorder { border-color:  var(--vscode-statusBar-debuggingBorder) !important; }.editor-inlineValuesForeground { color: var(--vscode-editor-inlineValuesForeground) !important; }.editor-inlineValuesBackground { background-color:  var(--vscode-editor-inlineValuesBackground) !important; }.editorGutter-modifiedBackground { background-color:  var(--vscode-editorGutter-modifiedBackground) !important; }.editorGutter-addedBackground { background-color:  var(--vscode-editorGutter-addedBackground) !important; }.editorGutter-deletedBackground { background-color:  var(--vscode-editorGutter-deletedBackground) !important; }.minimapGutter-modifiedBackground { background-color:  var(--vscode-minimapGutter-modifiedBackground) !important; }.minimapGutter-addedBackground { background-color:  var(--vscode-minimapGutter-addedBackground) !important; }.minimapGutter-deletedBackground { background-color:  var(--vscode-minimapGutter-deletedBackground) !important; }.editorOverviewRuler-modifiedForeground { color: var(--vscode-editorOverviewRuler-modifiedForeground) !important; }.editorOverviewRuler-addedForeground { color: var(--vscode-editorOverviewRuler-addedForeground) !important; }.editorOverviewRuler-deletedForeground { color: var(--vscode-editorOverviewRuler-deletedForeground) !important; }.debugIcon-breakpointForeground { color: var(--vscode-debugIcon-breakpointForeground) !important; }.debugIcon-breakpointDisabledForeground { color: var(--vscode-debugIcon-breakpointDisabledForeground) !important; }.debugIcon-breakpointUnverifiedForeground { color: var(--vscode-debugIcon-breakpointUnverifiedForeground) !important; }.debugIcon-breakpointCurrentStackframeForeground { color: var(--vscode-debugIcon-breakpointCurrentStackframeForeground) !important; }.debugIcon-breakpointStackframeForeground { color: var(--vscode-debugIcon-breakpointStackframeForeground) !important; }.notebook-cellBorderColor { border-color:  var(--vscode-notebook-cellBorderColor) !important; }.notebook-focusedEditorBorder { border-color:  var(--vscode-notebook-focusedEditorBorder) !important; }.notebookStatusSuccessIcon-foreground { color: var(--vscode-notebookStatusSuccessIcon-foreground) !important; }.notebookStatusErrorIcon-foreground { color: var(--vscode-notebookStatusErrorIcon-foreground) !important; }.notebookStatusRunningIcon-foreground { color: var(--vscode-notebookStatusRunningIcon-foreground) !important; }.notebook-outputContainerBorderColor { border-color:  var(--vscode-notebook-outputContainerBorderColor) !important; }.notebook-outputContainerBackgroundColor { background-color:  var(--vscode-notebook-outputContainerBackgroundColor) !important; }.notebook-focusedCellBackground { background-color:  var(--vscode-notebook-focusedCellBackground) !important; }.notebook-selectedCellBackground { background-color:  var(--vscode-notebook-selectedCellBackground) !important; }.notebook-cellHoverBackground { background-color:  var(--vscode-notebook-cellHoverBackground) !important; }.notebook-selectedCellBorder { border-color:  var(--vscode-notebook-selectedCellBorder) !important; }.notebook-inactiveSelectedCellBorder { border-color:  var(--vscode-notebook-inactiveSelectedCellBorder) !important; }.notebook-focusedCellBorder { border-color:  var(--vscode-notebook-focusedCellBorder) !important; }.notebook-inactiveFocusedCellBorder { border-color:  var(--vscode-notebook-inactiveFocusedCellBorder) !important; }.notebook-cellStatusBarItemHoverBackground { background-color:  var(--vscode-notebook-cellStatusBarItemHoverBackground) !important; }.notebookScrollbarSlider-background { background-color:  var(--vscode-notebookScrollbarSlider-background) !important; }.notebookScrollbarSlider-hoverBackground { background-color:  var(--vscode-notebookScrollbarSlider-hoverBackground) !important; }.notebookScrollbarSlider-activeBackground { background-color:  var(--vscode-notebookScrollbarSlider-activeBackground) !important; }.notebook-symbolHighlightBackground { background-color:  var(--vscode-notebook-symbolHighlightBackground) !important; }.notebook-cellEditorBackground { background-color:  var(--vscode-notebook-cellEditorBackground) !important; }.notebook-editorBackground { background-color:  var(--vscode-notebook-editorBackground) !important; }.keybindingTable-headerBackground { background-color:  var(--vscode-keybindingTable-headerBackground) !important; }.keybindingTable-rowsBackground { background-color:  var(--vscode-keybindingTable-rowsBackground) !important; }.scm-providerBorder { border-color:  var(--vscode-scm-providerBorder) !important; }.searchEditor-textInputBorder { border-color:  var(--vscode-searchEditor-textInputBorder) !important; }.debugView-exceptionLabelForeground { color: var(--vscode-debugView-exceptionLabelForeground) !important; }.debugView-exceptionLabelBackground { background-color:  var(--vscode-debugView-exceptionLabelBackground) !important; }.debugView-stateLabelForeground { color: var(--vscode-debugView-stateLabelForeground) !important; }.debugView-stateLabelBackground { background-color:  var(--vscode-debugView-stateLabelBackground) !important; }.debugConsole-infoForeground { color: var(--vscode-debugConsole-infoForeground) !important; }.debugConsole-warningForeground { color: var(--vscode-debugConsole-warningForeground) !important; }.debugConsole-errorForeground { color: var(--vscode-debugConsole-errorForeground) !important; }.debugConsole-sourceForeground { color: var(--vscode-debugConsole-sourceForeground) !important; }.debugConsoleInputIcon-foreground { color: var(--vscode-debugConsoleInputIcon-foreground) !important; }.debugIcon-pauseForeground { color: var(--vscode-debugIcon-pauseForeground) !important; }.debugIcon-stopForeground { color: var(--vscode-debugIcon-stopForeground) !important; }.debugIcon-disconnectForeground { color: var(--vscode-debugIcon-disconnectForeground) !important; }.debugIcon-restartForeground { color: var(--vscode-debugIcon-restartForeground) !important; }.debugIcon-stepOverForeground { color: var(--vscode-debugIcon-stepOverForeground) !important; }.debugIcon-stepIntoForeground { color: var(--vscode-debugIcon-stepIntoForeground) !important; }.debugIcon-stepOutForeground { color: var(--vscode-debugIcon-stepOutForeground) !important; }.debugIcon-continueForeground { color: var(--vscode-debugIcon-continueForeground) !important; }.debugIcon-stepBackForeground { color: var(--vscode-debugIcon-stepBackForeground) !important; }.extensionButton-background { background-color:  var(--vscode-extensionButton-background) !important; }.extensionButton-foreground { color: var(--vscode-extensionButton-foreground) !important; }.extensionButton-hoverBackground { background-color:  var(--vscode-extensionButton-hoverBackground) !important; }.extensionButton-prominentBackground { background-color:  var(--vscode-extensionButton-prominentBackground) !important; }.extensionButton-prominentForeground { color: var(--vscode-extensionButton-prominentForeground) !important; }.extensionButton-prominentHoverBackground { background-color:  var(--vscode-extensionButton-prominentHoverBackground) !important; }.extensionIcon-starForeground { color: var(--vscode-extensionIcon-starForeground) !important; }.extensionIcon-verifiedForeground { color: var(--vscode-extensionIcon-verifiedForeground) !important; }.extensionIcon-preReleaseForeground { color: var(--vscode-extensionIcon-preReleaseForeground) !important; }.extensionIcon-sponsorForeground { color: var(--vscode-extensionIcon-sponsorForeground) !important; }.interactive-activeCodeBorder { border-color:  var(--vscode-interactive-activeCodeBorder) !important; }.interactive-inactiveCodeBorder { border-color:  var(--vscode-interactive-inactiveCodeBorder) !important; }.gitDecoration-addedResourceForeground { color: var(--vscode-gitDecoration-addedResourceForeground) !important; }.gitDecoration-modifiedResourceForeground { color: var(--vscode-gitDecoration-modifiedResourceForeground) !important; }.gitDecoration-deletedResourceForeground { color: var(--vscode-gitDecoration-deletedResourceForeground) !important; }.gitDecoration-renamedResourceForeground { color: var(--vscode-gitDecoration-renamedResourceForeground) !important; }.gitDecoration-untrackedResourceForeground { color: var(--vscode-gitDecoration-untrackedResourceForeground) !important; }.gitDecoration-ignoredResourceForeground { color: var(--vscode-gitDecoration-ignoredResourceForeground) !important; }.gitDecoration-stageModifiedResourceForeground { color: var(--vscode-gitDecoration-stageModifiedResourceForeground) !important; }.gitDecoration-stageDeletedResourceForeground { color: var(--vscode-gitDecoration-stageDeletedResourceForeground) !important; }.gitDecoration-conflictingResourceForeground { color: var(--vscode-gitDecoration-conflictingResourceForeground) !important; }.gitDecoration-submoduleResourceForeground { color: var(--vscode-gitDecoration-submoduleResourceForeground) !important; }.gitlens-gutterBackgroundColor { background-color:  var(--vscode-gitlens-gutterBackgroundColor) !important; }.gitlens-gutterForegroundColor { color: var(--vscode-gitlens-gutterForegroundColor) !important; }.gitlens-gutterUncommittedForegroundColor { color: var(--vscode-gitlens-gutterUncommittedForegroundColor) !important; }.gitlens-trailingLineBackgroundColor { background-color:  var(--vscode-gitlens-trailingLineBackgroundColor) !important; }.gitlens-trailingLineForegroundColor { color: var(--vscode-gitlens-trailingLineForegroundColor) !important; }.gitlens-lineHighlightBackgroundColor { background-color:  var(--vscode-gitlens-lineHighlightBackgroundColor) !important; }.gitlens-decorations-addedForegroundColor { color: var(--vscode-gitlens-decorations-addedForegroundColor) !important; }.gitlens-decorations-copiedForegroundColor { color: var(--vscode-gitlens-decorations-copiedForegroundColor) !important; }.gitlens-decorations-deletedForegroundColor { color: var(--vscode-gitlens-decorations-deletedForegroundColor) !important; }.gitlens-decorations-ignoredForegroundColor { color: var(--vscode-gitlens-decorations-ignoredForegroundColor) !important; }.gitlens-decorations-modifiedForegroundColor { color: var(--vscode-gitlens-decorations-modifiedForegroundColor) !important; }.gitlens-decorations-untrackedForegroundColor { color: var(--vscode-gitlens-decorations-untrackedForegroundColor) !important; }.gitlens-decorations-renamedForegroundColor { color: var(--vscode-gitlens-decorations-renamedForegroundColor) !important; }.gitlens-decorations-branchAheadForegroundColor { color: var(--vscode-gitlens-decorations-branchAheadForegroundColor) !important; }.gitlens-decorations-branchBehindForegroundColor { color: var(--vscode-gitlens-decorations-branchBehindForegroundColor) !important; }.gitlens-decorations-branchDivergedForegroundColor { color: var(--vscode-gitlens-decorations-branchDivergedForegroundColor) !important; }.gitlens-decorations-branchUpToDateForegroundColor { color: var(--vscode-gitlens-decorations-branchUpToDateForegroundColor) !important; }.gitlens-decorations-branchUnpublishedForegroundColor { color: var(--vscode-gitlens-decorations-branchUnpublishedForegroundColor) !important; }.gitlens-decorations-branchMissingUpstreamForegroundColor { color: var(--vscode-gitlens-decorations-branchMissingUpstreamForegroundColor) !important; }.gitlens-decorations-worktreeView-hasUncommittedChangesForegroundColor { color: var(--vscode-gitlens-decorations-worktreeView-hasUncommittedChangesForegroundColor) !important; }"""


global =
    { foreground = Element.htmlAttribute (Attr.class "foreground")
    , disabledForeground = Element.htmlAttribute (Attr.class "disabledForeground")
    , errorForeground = Element.htmlAttribute (Attr.class "errorForeground")
    , descriptionForeground = Element.htmlAttribute (Attr.class "descriptionForeground")
    , focusBorder = Element.htmlAttribute (Attr.class "focusBorder")
    , contrastBorder = Element.htmlAttribute (Attr.class "contrastBorder")
    , contrastActiveBorder = Element.htmlAttribute (Attr.class "contrastActiveBorder")
    }


icon =
    { foreground = Element.htmlAttribute (Attr.class "icon-foreground")
    }


selection =
    { background = Element.htmlAttribute (Attr.class "selection-background")
    }


textSeparator =
    { foreground = Element.htmlAttribute (Attr.class "textSeparator-foreground")
    }


textLink =
    { foreground = Element.htmlAttribute (Attr.class "textLink-foreground")
    , activeForeground = Element.htmlAttribute (Attr.class "textLink-activeForeground")
    }


textPreformat =
    { foreground = Element.htmlAttribute (Attr.class "textPreformat-foreground")
    }


textBlockQuote =
    { background = Element.htmlAttribute (Attr.class "textBlockQuote-background")
    , border = Element.htmlAttribute (Attr.class "textBlockQuote-border")
    }


textCodeBlock =
    { background = Element.htmlAttribute (Attr.class "textCodeBlock-background")
    }


widget =
    { shadow = Element.htmlAttribute (Attr.class "widget-shadow")
    }


input =
    { background = Element.htmlAttribute (Attr.class "input-background")
    , foreground = Element.htmlAttribute (Attr.class "input-foreground")
    , border = Element.htmlAttribute (Attr.class "input-border")
    , placeholderForeground = Element.htmlAttribute (Attr.class "input-placeholderForeground")
    }


inputOption =
    { activeBorder = Element.htmlAttribute (Attr.class "inputOption-activeBorder")
    , hoverBackground = Element.htmlAttribute (Attr.class "inputOption-hoverBackground")
    , activeBackground = Element.htmlAttribute (Attr.class "inputOption-activeBackground")
    , activeForeground = Element.htmlAttribute (Attr.class "inputOption-activeForeground")
    }


inputValidation =
    { infoBackground = Element.htmlAttribute (Attr.class "inputValidation-infoBackground")
    , infoForeground = Element.htmlAttribute (Attr.class "inputValidation-infoForeground")
    , infoBorder = Element.htmlAttribute (Attr.class "inputValidation-infoBorder")
    , warningBackground = Element.htmlAttribute (Attr.class "inputValidation-warningBackground")
    , warningForeground = Element.htmlAttribute (Attr.class "inputValidation-warningForeground")
    , warningBorder = Element.htmlAttribute (Attr.class "inputValidation-warningBorder")
    , errorBackground = Element.htmlAttribute (Attr.class "inputValidation-errorBackground")
    , errorForeground = Element.htmlAttribute (Attr.class "inputValidation-errorForeground")
    , errorBorder = Element.htmlAttribute (Attr.class "inputValidation-errorBorder")
    }


dropdown =
    { background = Element.htmlAttribute (Attr.class "dropdown-background")
    , listBackground = Element.htmlAttribute (Attr.class "dropdown-listBackground")
    , foreground = Element.htmlAttribute (Attr.class "dropdown-foreground")
    , border = Element.htmlAttribute (Attr.class "dropdown-border")
    }


button =
    { foreground = Element.htmlAttribute (Attr.class "button-foreground")
    , separator = Element.htmlAttribute (Attr.class "button-separator")
    , background = Element.htmlAttribute (Attr.class "button-background")
    , hoverBackground = Element.htmlAttribute (Attr.class "button-hoverBackground")
    , border = Element.htmlAttribute (Attr.class "button-border")
    , secondaryForeground = Element.htmlAttribute (Attr.class "button-secondaryForeground")
    , secondaryBackground = Element.htmlAttribute (Attr.class "button-secondaryBackground")
    , secondaryHoverBackground = Element.htmlAttribute (Attr.class "button-secondaryHoverBackground")
    }


badge =
    { background = Element.htmlAttribute (Attr.class "badge-background")
    , foreground = Element.htmlAttribute (Attr.class "badge-foreground")
    }


scrollbar =
    { shadow = Element.htmlAttribute (Attr.class "scrollbar-shadow")
    }


scrollbarSlider =
    { background = Element.htmlAttribute (Attr.class "scrollbarSlider-background")
    , hoverBackground = Element.htmlAttribute (Attr.class "scrollbarSlider-hoverBackground")
    , activeBackground = Element.htmlAttribute (Attr.class "scrollbarSlider-activeBackground")
    }


progressBar =
    { background = Element.htmlAttribute (Attr.class "progressBar-background")
    }


editorError =
    { background = Element.htmlAttribute (Attr.class "editorError-background")
    , foreground = Element.htmlAttribute (Attr.class "editorError-foreground")
    , border = Element.htmlAttribute (Attr.class "editorError-border")
    }


editorWarning =
    { background = Element.htmlAttribute (Attr.class "editorWarning-background")
    , foreground = Element.htmlAttribute (Attr.class "editorWarning-foreground")
    , border = Element.htmlAttribute (Attr.class "editorWarning-border")
    }


editorInfo =
    { background = Element.htmlAttribute (Attr.class "editorInfo-background")
    , foreground = Element.htmlAttribute (Attr.class "editorInfo-foreground")
    , border = Element.htmlAttribute (Attr.class "editorInfo-border")
    }


editorHint =
    { foreground = Element.htmlAttribute (Attr.class "editorHint-foreground")
    , border = Element.htmlAttribute (Attr.class "editorHint-border")
    }


sash =
    { hoverBorder = Element.htmlAttribute (Attr.class "sash-hoverBorder")
    }


editor =
    { background = Element.htmlAttribute (Attr.class "editor-background")
    , foreground = Element.htmlAttribute (Attr.class "editor-foreground")
    , selectionBackground = Element.htmlAttribute (Attr.class "editor-selectionBackground")
    , selectionForeground = Element.htmlAttribute (Attr.class "editor-selectionForeground")
    , inactiveSelectionBackground = Element.htmlAttribute (Attr.class "editor-inactiveSelectionBackground")
    , selectionHighlightBackground = Element.htmlAttribute (Attr.class "editor-selectionHighlightBackground")
    , selectionHighlightBorder = Element.htmlAttribute (Attr.class "editor-selectionHighlightBorder")
    , findMatchBackground = Element.htmlAttribute (Attr.class "editor-findMatchBackground")
    , findMatchHighlightBackground = Element.htmlAttribute (Attr.class "editor-findMatchHighlightBackground")
    , findRangeHighlightBackground = Element.htmlAttribute (Attr.class "editor-findRangeHighlightBackground")
    , findMatchBorder = Element.htmlAttribute (Attr.class "editor-findMatchBorder")
    , findMatchHighlightBorder = Element.htmlAttribute (Attr.class "editor-findMatchHighlightBorder")
    , findRangeHighlightBorder = Element.htmlAttribute (Attr.class "editor-findRangeHighlightBorder")
    , hoverHighlightBackground = Element.htmlAttribute (Attr.class "editor-hoverHighlightBackground")
    , snippetTabstopHighlightBackground = Element.htmlAttribute (Attr.class "editor-snippetTabstopHighlightBackground")
    , snippetTabstopHighlightBorder = Element.htmlAttribute (Attr.class "editor-snippetTabstopHighlightBorder")
    , snippetFinalTabstopHighlightBackground = Element.htmlAttribute (Attr.class "editor-snippetFinalTabstopHighlightBackground")
    , snippetFinalTabstopHighlightBorder = Element.htmlAttribute (Attr.class "editor-snippetFinalTabstopHighlightBorder")
    , lineHighlightBackground = Element.htmlAttribute (Attr.class "editor-lineHighlightBackground")
    , lineHighlightBorder = Element.htmlAttribute (Attr.class "editor-lineHighlightBorder")
    , rangeHighlightBackground = Element.htmlAttribute (Attr.class "editor-rangeHighlightBackground")
    , rangeHighlightBorder = Element.htmlAttribute (Attr.class "editor-rangeHighlightBorder")
    , symbolHighlightBackground = Element.htmlAttribute (Attr.class "editor-symbolHighlightBackground")
    , symbolHighlightBorder = Element.htmlAttribute (Attr.class "editor-symbolHighlightBorder")
    , foldBackground = Element.htmlAttribute (Attr.class "editor-foldBackground")
    , linkedEditingBackground = Element.htmlAttribute (Attr.class "editor-linkedEditingBackground")
    , wordHighlightBackground = Element.htmlAttribute (Attr.class "editor-wordHighlightBackground")
    , wordHighlightStrongBackground = Element.htmlAttribute (Attr.class "editor-wordHighlightStrongBackground")
    , wordHighlightBorder = Element.htmlAttribute (Attr.class "editor-wordHighlightBorder")
    , wordHighlightStrongBorder = Element.htmlAttribute (Attr.class "editor-wordHighlightStrongBorder")
    , stackFrameHighlightBackground = Element.htmlAttribute (Attr.class "editor-stackFrameHighlightBackground")
    , focusedStackFrameHighlightBackground = Element.htmlAttribute (Attr.class "editor-focusedStackFrameHighlightBackground")
    , inlineValuesForeground = Element.htmlAttribute (Attr.class "editor-inlineValuesForeground")
    , inlineValuesBackground = Element.htmlAttribute (Attr.class "editor-inlineValuesBackground")
    }


editorStickyScroll =
    { background = Element.htmlAttribute (Attr.class "editorStickyScroll-background")
    }


editorStickyScrollHover =
    { background = Element.htmlAttribute (Attr.class "editorStickyScrollHover-background")
    }


editorWidget =
    { background = Element.htmlAttribute (Attr.class "editorWidget-background")
    , foreground = Element.htmlAttribute (Attr.class "editorWidget-foreground")
    , border = Element.htmlAttribute (Attr.class "editorWidget-border")
    , resizeBorder = Element.htmlAttribute (Attr.class "editorWidget-resizeBorder")
    }


quickInput =
    { background = Element.htmlAttribute (Attr.class "quickInput-background")
    , foreground = Element.htmlAttribute (Attr.class "quickInput-foreground")
    , list_focusBackground = Element.htmlAttribute (Attr.class "quickInput-list-focusBackground")
    }


quickInputTitle =
    { background = Element.htmlAttribute (Attr.class "quickInputTitle-background")
    }


pickerGroup =
    { foreground = Element.htmlAttribute (Attr.class "pickerGroup-foreground")
    , border = Element.htmlAttribute (Attr.class "pickerGroup-border")
    }


keybindingLabel =
    { background = Element.htmlAttribute (Attr.class "keybindingLabel-background")
    , foreground = Element.htmlAttribute (Attr.class "keybindingLabel-foreground")
    , border = Element.htmlAttribute (Attr.class "keybindingLabel-border")
    , bottomBorder = Element.htmlAttribute (Attr.class "keybindingLabel-bottomBorder")
    }


searchEditor =
    { findMatchBackground = Element.htmlAttribute (Attr.class "searchEditor-findMatchBackground")
    , findMatchBorder = Element.htmlAttribute (Attr.class "searchEditor-findMatchBorder")
    , textInputBorder = Element.htmlAttribute (Attr.class "searchEditor-textInputBorder")
    }


editorHoverWidget =
    { background = Element.htmlAttribute (Attr.class "editorHoverWidget-background")
    , foreground = Element.htmlAttribute (Attr.class "editorHoverWidget-foreground")
    , border = Element.htmlAttribute (Attr.class "editorHoverWidget-border")
    , statusBarBackground = Element.htmlAttribute (Attr.class "editorHoverWidget-statusBarBackground")
    , highlightForeground = Element.htmlAttribute (Attr.class "editorHoverWidget-highlightForeground")
    }


editorLink =
    { activeForeground = Element.htmlAttribute (Attr.class "editorLink-activeForeground")
    }


editorInlayHint =
    { foreground = Element.htmlAttribute (Attr.class "editorInlayHint-foreground")
    , background = Element.htmlAttribute (Attr.class "editorInlayHint-background")
    , typeForeground = Element.htmlAttribute (Attr.class "editorInlayHint-typeForeground")
    , typeBackground = Element.htmlAttribute (Attr.class "editorInlayHint-typeBackground")
    , parameterForeground = Element.htmlAttribute (Attr.class "editorInlayHint-parameterForeground")
    , parameterBackground = Element.htmlAttribute (Attr.class "editorInlayHint-parameterBackground")
    }


editorLightBulb =
    { foreground = Element.htmlAttribute (Attr.class "editorLightBulb-foreground")
    }


editorLightBulbAutoFix =
    { foreground = Element.htmlAttribute (Attr.class "editorLightBulbAutoFix-foreground")
    }


diffEditor =
    { insertedTextBackground = Element.htmlAttribute (Attr.class "diffEditor-insertedTextBackground")
    , removedTextBackground = Element.htmlAttribute (Attr.class "diffEditor-removedTextBackground")
    , insertedLineBackground = Element.htmlAttribute (Attr.class "diffEditor-insertedLineBackground")
    , removedLineBackground = Element.htmlAttribute (Attr.class "diffEditor-removedLineBackground")
    , insertedTextBorder = Element.htmlAttribute (Attr.class "diffEditor-insertedTextBorder")
    , removedTextBorder = Element.htmlAttribute (Attr.class "diffEditor-removedTextBorder")
    , border = Element.htmlAttribute (Attr.class "diffEditor-border")
    , diagonalFill = Element.htmlAttribute (Attr.class "diffEditor-diagonalFill")
    }


diffEditorGutter =
    { insertedLineBackground = Element.htmlAttribute (Attr.class "diffEditorGutter-insertedLineBackground")
    , removedLineBackground = Element.htmlAttribute (Attr.class "diffEditorGutter-removedLineBackground")
    }


diffEditorOverview =
    { insertedForeground = Element.htmlAttribute (Attr.class "diffEditorOverview-insertedForeground")
    , removedForeground = Element.htmlAttribute (Attr.class "diffEditorOverview-removedForeground")
    }


list =
    { focusBackground = Element.htmlAttribute (Attr.class "list-focusBackground")
    , focusForeground = Element.htmlAttribute (Attr.class "list-focusForeground")
    , focusOutline = Element.htmlAttribute (Attr.class "list-focusOutline")
    , focusAndSelectionOutline = Element.htmlAttribute (Attr.class "list-focusAndSelectionOutline")
    , activeSelectionBackground = Element.htmlAttribute (Attr.class "list-activeSelectionBackground")
    , activeSelectionForeground = Element.htmlAttribute (Attr.class "list-activeSelectionForeground")
    , activeSelectionIconForeground = Element.htmlAttribute (Attr.class "list-activeSelectionIconForeground")
    , inactiveSelectionBackground = Element.htmlAttribute (Attr.class "list-inactiveSelectionBackground")
    , inactiveSelectionForeground = Element.htmlAttribute (Attr.class "list-inactiveSelectionForeground")
    , inactiveSelectionIconForeground = Element.htmlAttribute (Attr.class "list-inactiveSelectionIconForeground")
    , inactiveFocusBackground = Element.htmlAttribute (Attr.class "list-inactiveFocusBackground")
    , inactiveFocusOutline = Element.htmlAttribute (Attr.class "list-inactiveFocusOutline")
    , hoverBackground = Element.htmlAttribute (Attr.class "list-hoverBackground")
    , hoverForeground = Element.htmlAttribute (Attr.class "list-hoverForeground")
    , dropBackground = Element.htmlAttribute (Attr.class "list-dropBackground")
    , highlightForeground = Element.htmlAttribute (Attr.class "list-highlightForeground")
    , focusHighlightForeground = Element.htmlAttribute (Attr.class "list-focusHighlightForeground")
    , invalidItemForeground = Element.htmlAttribute (Attr.class "list-invalidItemForeground")
    , errorForeground = Element.htmlAttribute (Attr.class "list-errorForeground")
    , warningForeground = Element.htmlAttribute (Attr.class "list-warningForeground")
    , filterMatchBackground = Element.htmlAttribute (Attr.class "list-filterMatchBackground")
    , filterMatchBorder = Element.htmlAttribute (Attr.class "list-filterMatchBorder")
    , deemphasizedForeground = Element.htmlAttribute (Attr.class "list-deemphasizedForeground")
    }


listFilterWidget =
    { background = Element.htmlAttribute (Attr.class "listFilterWidget-background")
    , outline = Element.htmlAttribute (Attr.class "listFilterWidget-outline")
    , noMatchesOutline = Element.htmlAttribute (Attr.class "listFilterWidget-noMatchesOutline")
    , shadow = Element.htmlAttribute (Attr.class "listFilterWidget-shadow")
    }


tree =
    { indentGuidesStroke = Element.htmlAttribute (Attr.class "tree-indentGuidesStroke")
    , tableColumnsBorder = Element.htmlAttribute (Attr.class "tree-tableColumnsBorder")
    , tableOddRowsBackground = Element.htmlAttribute (Attr.class "tree-tableOddRowsBackground")
    }


checkbox =
    { background = Element.htmlAttribute (Attr.class "checkbox-background")
    , selectBackground = Element.htmlAttribute (Attr.class "checkbox-selectBackground")
    , foreground = Element.htmlAttribute (Attr.class "checkbox-foreground")
    , border = Element.htmlAttribute (Attr.class "checkbox-border")
    , selectBorder = Element.htmlAttribute (Attr.class "checkbox-selectBorder")
    }


quickInputList =
    { focusForeground = Element.htmlAttribute (Attr.class "quickInputList-focusForeground")
    , focusIconForeground = Element.htmlAttribute (Attr.class "quickInputList-focusIconForeground")
    , focusBackground = Element.htmlAttribute (Attr.class "quickInputList-focusBackground")
    }


menu =
    { border = Element.htmlAttribute (Attr.class "menu-border")
    , foreground = Element.htmlAttribute (Attr.class "menu-foreground")
    , background = Element.htmlAttribute (Attr.class "menu-background")
    , selectionForeground = Element.htmlAttribute (Attr.class "menu-selectionForeground")
    , selectionBackground = Element.htmlAttribute (Attr.class "menu-selectionBackground")
    , selectionBorder = Element.htmlAttribute (Attr.class "menu-selectionBorder")
    , separatorBackground = Element.htmlAttribute (Attr.class "menu-separatorBackground")
    }


toolbar =
    { hoverBackground = Element.htmlAttribute (Attr.class "toolbar-hoverBackground")
    , hoverOutline = Element.htmlAttribute (Attr.class "toolbar-hoverOutline")
    , activeBackground = Element.htmlAttribute (Attr.class "toolbar-activeBackground")
    }


breadcrumb =
    { foreground = Element.htmlAttribute (Attr.class "breadcrumb-foreground")
    , background = Element.htmlAttribute (Attr.class "breadcrumb-background")
    , focusForeground = Element.htmlAttribute (Attr.class "breadcrumb-focusForeground")
    , activeSelectionForeground = Element.htmlAttribute (Attr.class "breadcrumb-activeSelectionForeground")
    }


breadcrumbPicker =
    { background = Element.htmlAttribute (Attr.class "breadcrumbPicker-background")
    }


merge =
    { currentHeaderBackground = Element.htmlAttribute (Attr.class "merge-currentHeaderBackground")
    , currentContentBackground = Element.htmlAttribute (Attr.class "merge-currentContentBackground")
    , incomingHeaderBackground = Element.htmlAttribute (Attr.class "merge-incomingHeaderBackground")
    , incomingContentBackground = Element.htmlAttribute (Attr.class "merge-incomingContentBackground")
    , commonHeaderBackground = Element.htmlAttribute (Attr.class "merge-commonHeaderBackground")
    , commonContentBackground = Element.htmlAttribute (Attr.class "merge-commonContentBackground")
    , border = Element.htmlAttribute (Attr.class "merge-border")
    }


editorOverviewRuler =
    { currentContentForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-currentContentForeground")
    , incomingContentForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-incomingContentForeground")
    , commonContentForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-commonContentForeground")
    , findMatchForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-findMatchForeground")
    , selectionHighlightForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-selectionHighlightForeground")
    , border = Element.htmlAttribute (Attr.class "editorOverviewRuler-border")
    , background = Element.htmlAttribute (Attr.class "editorOverviewRuler-background")
    , rangeHighlightForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-rangeHighlightForeground")
    , errorForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-errorForeground")
    , warningForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-warningForeground")
    , infoForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-infoForeground")
    , bracketMatchForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-bracketMatchForeground")
    , wordHighlightForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-wordHighlightForeground")
    , wordHighlightStrongForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-wordHighlightStrongForeground")
    , modifiedForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-modifiedForeground")
    , addedForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-addedForeground")
    , deletedForeground = Element.htmlAttribute (Attr.class "editorOverviewRuler-deletedForeground")
    }


minimap =
    { findMatchHighlight = Element.htmlAttribute (Attr.class "minimap-findMatchHighlight")
    , selectionOccurrenceHighlight = Element.htmlAttribute (Attr.class "minimap-selectionOccurrenceHighlight")
    , selectionHighlight = Element.htmlAttribute (Attr.class "minimap-selectionHighlight")
    , errorHighlight = Element.htmlAttribute (Attr.class "minimap-errorHighlight")
    , warningHighlight = Element.htmlAttribute (Attr.class "minimap-warningHighlight")
    , background = Element.htmlAttribute (Attr.class "minimap-background")
    , foregroundOpacity = Element.htmlAttribute (Attr.class "minimap-foregroundOpacity")
    }


minimapSlider =
    { background = Element.htmlAttribute (Attr.class "minimapSlider-background")
    , hoverBackground = Element.htmlAttribute (Attr.class "minimapSlider-hoverBackground")
    , activeBackground = Element.htmlAttribute (Attr.class "minimapSlider-activeBackground")
    }


problemsErrorIcon =
    { foreground = Element.htmlAttribute (Attr.class "problemsErrorIcon-foreground")
    }


problemsWarningIcon =
    { foreground = Element.htmlAttribute (Attr.class "problemsWarningIcon-foreground")
    }


problemsInfoIcon =
    { foreground = Element.htmlAttribute (Attr.class "problemsInfoIcon-foreground")
    }


charts =
    { foreground = Element.htmlAttribute (Attr.class "charts-foreground")
    , lines = Element.htmlAttribute (Attr.class "charts-lines")
    , red = Element.htmlAttribute (Attr.class "charts-red")
    , blue = Element.htmlAttribute (Attr.class "charts-blue")
    , yellow = Element.htmlAttribute (Attr.class "charts-yellow")
    , orange = Element.htmlAttribute (Attr.class "charts-orange")
    , green = Element.htmlAttribute (Attr.class "charts-green")
    , purple = Element.htmlAttribute (Attr.class "charts-purple")
    }


symbolIcon =
    { arrayForeground = Element.htmlAttribute (Attr.class "symbolIcon-arrayForeground")
    , booleanForeground = Element.htmlAttribute (Attr.class "symbolIcon-booleanForeground")
    , classForeground = Element.htmlAttribute (Attr.class "symbolIcon-classForeground")
    , colorForeground = Element.htmlAttribute (Attr.class "symbolIcon-colorForeground")
    , constantForeground = Element.htmlAttribute (Attr.class "symbolIcon-constantForeground")
    , constructorForeground = Element.htmlAttribute (Attr.class "symbolIcon-constructorForeground")
    , enumeratorForeground = Element.htmlAttribute (Attr.class "symbolIcon-enumeratorForeground")
    , enumeratorMemberForeground = Element.htmlAttribute (Attr.class "symbolIcon-enumeratorMemberForeground")
    , eventForeground = Element.htmlAttribute (Attr.class "symbolIcon-eventForeground")
    , fieldForeground = Element.htmlAttribute (Attr.class "symbolIcon-fieldForeground")
    , fileForeground = Element.htmlAttribute (Attr.class "symbolIcon-fileForeground")
    , folderForeground = Element.htmlAttribute (Attr.class "symbolIcon-folderForeground")
    , functionForeground = Element.htmlAttribute (Attr.class "symbolIcon-functionForeground")
    , interfaceForeground = Element.htmlAttribute (Attr.class "symbolIcon-interfaceForeground")
    , keyForeground = Element.htmlAttribute (Attr.class "symbolIcon-keyForeground")
    , keywordForeground = Element.htmlAttribute (Attr.class "symbolIcon-keywordForeground")
    , methodForeground = Element.htmlAttribute (Attr.class "symbolIcon-methodForeground")
    , moduleForeground = Element.htmlAttribute (Attr.class "symbolIcon-moduleForeground")
    , namespaceForeground = Element.htmlAttribute (Attr.class "symbolIcon-namespaceForeground")
    , nullForeground = Element.htmlAttribute (Attr.class "symbolIcon-nullForeground")
    , numberForeground = Element.htmlAttribute (Attr.class "symbolIcon-numberForeground")
    , objectForeground = Element.htmlAttribute (Attr.class "symbolIcon-objectForeground")
    , operatorForeground = Element.htmlAttribute (Attr.class "symbolIcon-operatorForeground")
    , packageForeground = Element.htmlAttribute (Attr.class "symbolIcon-packageForeground")
    , propertyForeground = Element.htmlAttribute (Attr.class "symbolIcon-propertyForeground")
    , referenceForeground = Element.htmlAttribute (Attr.class "symbolIcon-referenceForeground")
    , snippetForeground = Element.htmlAttribute (Attr.class "symbolIcon-snippetForeground")
    , stringForeground = Element.htmlAttribute (Attr.class "symbolIcon-stringForeground")
    , structForeground = Element.htmlAttribute (Attr.class "symbolIcon-structForeground")
    , textForeground = Element.htmlAttribute (Attr.class "symbolIcon-textForeground")
    , typeParameterForeground = Element.htmlAttribute (Attr.class "symbolIcon-typeParameterForeground")
    , unitForeground = Element.htmlAttribute (Attr.class "symbolIcon-unitForeground")
    , variableForeground = Element.htmlAttribute (Attr.class "symbolIcon-variableForeground")
    }


editorCursor =
    { foreground = Element.htmlAttribute (Attr.class "editorCursor-foreground")
    , background = Element.htmlAttribute (Attr.class "editorCursor-background")
    }


editorWhitespace =
    { foreground = Element.htmlAttribute (Attr.class "editorWhitespace-foreground")
    }


editorIndentGuide =
    { background = Element.htmlAttribute (Attr.class "editorIndentGuide-background")
    , activeBackground = Element.htmlAttribute (Attr.class "editorIndentGuide-activeBackground")
    }


editorLineNumber =
    { foreground = Element.htmlAttribute (Attr.class "editorLineNumber-foreground")
    , activeForeground = Element.htmlAttribute (Attr.class "editorLineNumber-activeForeground")
    }


editorActiveLineNumber =
    { foreground = Element.htmlAttribute (Attr.class "editorActiveLineNumber-foreground")
    }


editorRuler =
    { foreground = Element.htmlAttribute (Attr.class "editorRuler-foreground")
    }


editorCodeLens =
    { foreground = Element.htmlAttribute (Attr.class "editorCodeLens-foreground")
    }


editorBracketMatch =
    { background = Element.htmlAttribute (Attr.class "editorBracketMatch-background")
    , border = Element.htmlAttribute (Attr.class "editorBracketMatch-border")
    }


editorGutter =
    { background = Element.htmlAttribute (Attr.class "editorGutter-background")
    , foldingControlForeground = Element.htmlAttribute (Attr.class "editorGutter-foldingControlForeground")
    , commentRangeForeground = Element.htmlAttribute (Attr.class "editorGutter-commentRangeForeground")
    , modifiedBackground = Element.htmlAttribute (Attr.class "editorGutter-modifiedBackground")
    , addedBackground = Element.htmlAttribute (Attr.class "editorGutter-addedBackground")
    , deletedBackground = Element.htmlAttribute (Attr.class "editorGutter-deletedBackground")
    }


editorUnnecessaryCode =
    { border = Element.htmlAttribute (Attr.class "editorUnnecessaryCode-border")
    , opacity = Element.htmlAttribute (Attr.class "editorUnnecessaryCode-opacity")
    }


editorGhostText =
    { border = Element.htmlAttribute (Attr.class "editorGhostText-border")
    , foreground = Element.htmlAttribute (Attr.class "editorGhostText-foreground")
    , background = Element.htmlAttribute (Attr.class "editorGhostText-background")
    }


editorBracketHighlight =
    { foreground1 = Element.htmlAttribute (Attr.class "editorBracketHighlight-foreground1")
    , foreground2 = Element.htmlAttribute (Attr.class "editorBracketHighlight-foreground2")
    , foreground3 = Element.htmlAttribute (Attr.class "editorBracketHighlight-foreground3")
    , foreground4 = Element.htmlAttribute (Attr.class "editorBracketHighlight-foreground4")
    , foreground5 = Element.htmlAttribute (Attr.class "editorBracketHighlight-foreground5")
    , foreground6 = Element.htmlAttribute (Attr.class "editorBracketHighlight-foreground6")
    , unexpectedBracket_foreground = Element.htmlAttribute (Attr.class "editorBracketHighlight-unexpectedBracket-foreground")
    }


editorBracketPairGuide =
    { background1 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-background1")
    , background2 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-background2")
    , background3 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-background3")
    , background4 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-background4")
    , background5 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-background5")
    , background6 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-background6")
    , activeBackground1 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-activeBackground1")
    , activeBackground2 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-activeBackground2")
    , activeBackground3 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-activeBackground3")
    , activeBackground4 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-activeBackground4")
    , activeBackground5 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-activeBackground5")
    , activeBackground6 = Element.htmlAttribute (Attr.class "editorBracketPairGuide-activeBackground6")
    }


editorUnicodeHighlight =
    { border = Element.htmlAttribute (Attr.class "editorUnicodeHighlight-border")
    , background = Element.htmlAttribute (Attr.class "editorUnicodeHighlight-background")
    }


peekViewTitle =
    { background = Element.htmlAttribute (Attr.class "peekViewTitle-background")
    }


peekViewTitleLabel =
    { foreground = Element.htmlAttribute (Attr.class "peekViewTitleLabel-foreground")
    }


peekViewTitleDescription =
    { foreground = Element.htmlAttribute (Attr.class "peekViewTitleDescription-foreground")
    }


peekView =
    { border = Element.htmlAttribute (Attr.class "peekView-border")
    }


peekViewResult =
    { background = Element.htmlAttribute (Attr.class "peekViewResult-background")
    , lineForeground = Element.htmlAttribute (Attr.class "peekViewResult-lineForeground")
    , fileForeground = Element.htmlAttribute (Attr.class "peekViewResult-fileForeground")
    , selectionBackground = Element.htmlAttribute (Attr.class "peekViewResult-selectionBackground")
    , selectionForeground = Element.htmlAttribute (Attr.class "peekViewResult-selectionForeground")
    , matchHighlightBackground = Element.htmlAttribute (Attr.class "peekViewResult-matchHighlightBackground")
    }


peekViewEditor =
    { background = Element.htmlAttribute (Attr.class "peekViewEditor-background")
    , matchHighlightBackground = Element.htmlAttribute (Attr.class "peekViewEditor-matchHighlightBackground")
    , matchHighlightBorder = Element.htmlAttribute (Attr.class "peekViewEditor-matchHighlightBorder")
    }


peekViewEditorGutter =
    { background = Element.htmlAttribute (Attr.class "peekViewEditorGutter-background")
    }


editorMarkerNavigationError =
    { background = Element.htmlAttribute (Attr.class "editorMarkerNavigationError-background")
    , headerBackground = Element.htmlAttribute (Attr.class "editorMarkerNavigationError-headerBackground")
    }


editorMarkerNavigationWarning =
    { background = Element.htmlAttribute (Attr.class "editorMarkerNavigationWarning-background")
    , headerBackground = Element.htmlAttribute (Attr.class "editorMarkerNavigationWarning-headerBackground")
    }


editorMarkerNavigationInfo =
    { background = Element.htmlAttribute (Attr.class "editorMarkerNavigationInfo-background")
    , headerBackground = Element.htmlAttribute (Attr.class "editorMarkerNavigationInfo-headerBackground")
    }


editorMarkerNavigation =
    { background = Element.htmlAttribute (Attr.class "editorMarkerNavigation-background")
    }


editorSuggestWidget =
    { background = Element.htmlAttribute (Attr.class "editorSuggestWidget-background")
    , border = Element.htmlAttribute (Attr.class "editorSuggestWidget-border")
    , foreground = Element.htmlAttribute (Attr.class "editorSuggestWidget-foreground")
    , selectedForeground = Element.htmlAttribute (Attr.class "editorSuggestWidget-selectedForeground")
    , selectedIconForeground = Element.htmlAttribute (Attr.class "editorSuggestWidget-selectedIconForeground")
    , selectedBackground = Element.htmlAttribute (Attr.class "editorSuggestWidget-selectedBackground")
    , highlightForeground = Element.htmlAttribute (Attr.class "editorSuggestWidget-highlightForeground")
    , focusHighlightForeground = Element.htmlAttribute (Attr.class "editorSuggestWidget-focusHighlightForeground")
    }


editorSuggestWidgetStatus =
    { foreground = Element.htmlAttribute (Attr.class "editorSuggestWidgetStatus-foreground")
    }


tab =
    { activeBackground = Element.htmlAttribute (Attr.class "tab-activeBackground")
    , unfocusedActiveBackground = Element.htmlAttribute (Attr.class "tab-unfocusedActiveBackground")
    , inactiveBackground = Element.htmlAttribute (Attr.class "tab-inactiveBackground")
    , unfocusedInactiveBackground = Element.htmlAttribute (Attr.class "tab-unfocusedInactiveBackground")
    , activeForeground = Element.htmlAttribute (Attr.class "tab-activeForeground")
    , inactiveForeground = Element.htmlAttribute (Attr.class "tab-inactiveForeground")
    , unfocusedActiveForeground = Element.htmlAttribute (Attr.class "tab-unfocusedActiveForeground")
    , unfocusedInactiveForeground = Element.htmlAttribute (Attr.class "tab-unfocusedInactiveForeground")
    , hoverBackground = Element.htmlAttribute (Attr.class "tab-hoverBackground")
    , unfocusedHoverBackground = Element.htmlAttribute (Attr.class "tab-unfocusedHoverBackground")
    , hoverForeground = Element.htmlAttribute (Attr.class "tab-hoverForeground")
    , unfocusedHoverForeground = Element.htmlAttribute (Attr.class "tab-unfocusedHoverForeground")
    , border = Element.htmlAttribute (Attr.class "tab-border")
    , lastPinnedBorder = Element.htmlAttribute (Attr.class "tab-lastPinnedBorder")
    , activeBorder = Element.htmlAttribute (Attr.class "tab-activeBorder")
    , unfocusedActiveBorder = Element.htmlAttribute (Attr.class "tab-unfocusedActiveBorder")
    , activeBorderTop = Element.htmlAttribute (Attr.class "tab-activeBorderTop")
    , unfocusedActiveBorderTop = Element.htmlAttribute (Attr.class "tab-unfocusedActiveBorderTop")
    , hoverBorder = Element.htmlAttribute (Attr.class "tab-hoverBorder")
    , unfocusedHoverBorder = Element.htmlAttribute (Attr.class "tab-unfocusedHoverBorder")
    , activeModifiedBorder = Element.htmlAttribute (Attr.class "tab-activeModifiedBorder")
    , inactiveModifiedBorder = Element.htmlAttribute (Attr.class "tab-inactiveModifiedBorder")
    , unfocusedActiveModifiedBorder = Element.htmlAttribute (Attr.class "tab-unfocusedActiveModifiedBorder")
    , unfocusedInactiveModifiedBorder = Element.htmlAttribute (Attr.class "tab-unfocusedInactiveModifiedBorder")
    }


editorPane =
    { background = Element.htmlAttribute (Attr.class "editorPane-background")
    }


editorGroup =
    { emptyBackground = Element.htmlAttribute (Attr.class "editorGroup-emptyBackground")
    , focusedEmptyBorder = Element.htmlAttribute (Attr.class "editorGroup-focusedEmptyBorder")
    , border = Element.htmlAttribute (Attr.class "editorGroup-border")
    , dropBackground = Element.htmlAttribute (Attr.class "editorGroup-dropBackground")
    , dropIntoPromptForeground = Element.htmlAttribute (Attr.class "editorGroup-dropIntoPromptForeground")
    , dropIntoPromptBackground = Element.htmlAttribute (Attr.class "editorGroup-dropIntoPromptBackground")
    , dropIntoPromptBorder = Element.htmlAttribute (Attr.class "editorGroup-dropIntoPromptBorder")
    }


editorGroupHeader =
    { tabsBackground = Element.htmlAttribute (Attr.class "editorGroupHeader-tabsBackground")
    , tabsBorder = Element.htmlAttribute (Attr.class "editorGroupHeader-tabsBorder")
    , noTabsBackground = Element.htmlAttribute (Attr.class "editorGroupHeader-noTabsBackground")
    , border = Element.htmlAttribute (Attr.class "editorGroupHeader-border")
    }


sideBySideEditor =
    { horizontalBorder = Element.htmlAttribute (Attr.class "sideBySideEditor-horizontalBorder")
    , verticalBorder = Element.htmlAttribute (Attr.class "sideBySideEditor-verticalBorder")
    }


panel =
    { background = Element.htmlAttribute (Attr.class "panel-background")
    , border = Element.htmlAttribute (Attr.class "panel-border")
    , dropBorder = Element.htmlAttribute (Attr.class "panel-dropBorder")
    }


panelTitle =
    { activeForeground = Element.htmlAttribute (Attr.class "panelTitle-activeForeground")
    , inactiveForeground = Element.htmlAttribute (Attr.class "panelTitle-inactiveForeground")
    , activeBorder = Element.htmlAttribute (Attr.class "panelTitle-activeBorder")
    }


panelInput =
    { border = Element.htmlAttribute (Attr.class "panelInput-border")
    }


panelSection =
    { dropBackground = Element.htmlAttribute (Attr.class "panelSection-dropBackground")
    , border = Element.htmlAttribute (Attr.class "panelSection-border")
    }


panelSectionHeader =
    { background = Element.htmlAttribute (Attr.class "panelSectionHeader-background")
    , foreground = Element.htmlAttribute (Attr.class "panelSectionHeader-foreground")
    , border = Element.htmlAttribute (Attr.class "panelSectionHeader-border")
    }


banner =
    { background = Element.htmlAttribute (Attr.class "banner-background")
    , foreground = Element.htmlAttribute (Attr.class "banner-foreground")
    , iconForeground = Element.htmlAttribute (Attr.class "banner-iconForeground")
    }


statusBar =
    { foreground = Element.htmlAttribute (Attr.class "statusBar-foreground")
    , noFolderForeground = Element.htmlAttribute (Attr.class "statusBar-noFolderForeground")
    , background = Element.htmlAttribute (Attr.class "statusBar-background")
    , noFolderBackground = Element.htmlAttribute (Attr.class "statusBar-noFolderBackground")
    , border = Element.htmlAttribute (Attr.class "statusBar-border")
    , focusBorder = Element.htmlAttribute (Attr.class "statusBar-focusBorder")
    , noFolderBorder = Element.htmlAttribute (Attr.class "statusBar-noFolderBorder")
    , debuggingBackground = Element.htmlAttribute (Attr.class "statusBar-debuggingBackground")
    , debuggingForeground = Element.htmlAttribute (Attr.class "statusBar-debuggingForeground")
    , debuggingBorder = Element.htmlAttribute (Attr.class "statusBar-debuggingBorder")
    }


statusBarItem =
    { activeBackground = Element.htmlAttribute (Attr.class "statusBarItem-activeBackground")
    , focusBorder = Element.htmlAttribute (Attr.class "statusBarItem-focusBorder")
    , hoverBackground = Element.htmlAttribute (Attr.class "statusBarItem-hoverBackground")
    , compactHoverBackground = Element.htmlAttribute (Attr.class "statusBarItem-compactHoverBackground")
    , prominentForeground = Element.htmlAttribute (Attr.class "statusBarItem-prominentForeground")
    , prominentBackground = Element.htmlAttribute (Attr.class "statusBarItem-prominentBackground")
    , prominentHoverBackground = Element.htmlAttribute (Attr.class "statusBarItem-prominentHoverBackground")
    , errorBackground = Element.htmlAttribute (Attr.class "statusBarItem-errorBackground")
    , errorForeground = Element.htmlAttribute (Attr.class "statusBarItem-errorForeground")
    , warningBackground = Element.htmlAttribute (Attr.class "statusBarItem-warningBackground")
    , warningForeground = Element.htmlAttribute (Attr.class "statusBarItem-warningForeground")
    , remoteBackground = Element.htmlAttribute (Attr.class "statusBarItem-remoteBackground")
    , remoteForeground = Element.htmlAttribute (Attr.class "statusBarItem-remoteForeground")
    }


activityBar =
    { background = Element.htmlAttribute (Attr.class "activityBar-background")
    , foreground = Element.htmlAttribute (Attr.class "activityBar-foreground")
    , inactiveForeground = Element.htmlAttribute (Attr.class "activityBar-inactiveForeground")
    , border = Element.htmlAttribute (Attr.class "activityBar-border")
    , activeBorder = Element.htmlAttribute (Attr.class "activityBar-activeBorder")
    , activeFocusBorder = Element.htmlAttribute (Attr.class "activityBar-activeFocusBorder")
    , activeBackground = Element.htmlAttribute (Attr.class "activityBar-activeBackground")
    , dropBorder = Element.htmlAttribute (Attr.class "activityBar-dropBorder")
    }


activityBarBadge =
    { background = Element.htmlAttribute (Attr.class "activityBarBadge-background")
    , foreground = Element.htmlAttribute (Attr.class "activityBarBadge-foreground")
    }


activityBarItem =
    { profilesForeground = Element.htmlAttribute (Attr.class "activityBarItem-profilesForeground")
    , profilesHoverForeground = Element.htmlAttribute (Attr.class "activityBarItem-profilesHoverForeground")
    , profilesBackground = Element.htmlAttribute (Attr.class "activityBarItem-profilesBackground")
    }


extensionBadge =
    { remoteBackground = Element.htmlAttribute (Attr.class "extensionBadge-remoteBackground")
    , remoteForeground = Element.htmlAttribute (Attr.class "extensionBadge-remoteForeground")
    }


sideBar =
    { background = Element.htmlAttribute (Attr.class "sideBar-background")
    , foreground = Element.htmlAttribute (Attr.class "sideBar-foreground")
    , border = Element.htmlAttribute (Attr.class "sideBar-border")
    , dropBackground = Element.htmlAttribute (Attr.class "sideBar-dropBackground")
    }


sideBarTitle =
    { foreground = Element.htmlAttribute (Attr.class "sideBarTitle-foreground")
    }


sideBarSectionHeader =
    { background = Element.htmlAttribute (Attr.class "sideBarSectionHeader-background")
    , foreground = Element.htmlAttribute (Attr.class "sideBarSectionHeader-foreground")
    , border = Element.htmlAttribute (Attr.class "sideBarSectionHeader-border")
    }


titleBar =
    { activeForeground = Element.htmlAttribute (Attr.class "titleBar-activeForeground")
    , inactiveForeground = Element.htmlAttribute (Attr.class "titleBar-inactiveForeground")
    , activeBackground = Element.htmlAttribute (Attr.class "titleBar-activeBackground")
    , inactiveBackground = Element.htmlAttribute (Attr.class "titleBar-inactiveBackground")
    , border = Element.htmlAttribute (Attr.class "titleBar-border")
    }


menubar =
    { selectionForeground = Element.htmlAttribute (Attr.class "menubar-selectionForeground")
    , selectionBackground = Element.htmlAttribute (Attr.class "menubar-selectionBackground")
    , selectionBorder = Element.htmlAttribute (Attr.class "menubar-selectionBorder")
    }


notificationCenter =
    { border = Element.htmlAttribute (Attr.class "notificationCenter-border")
    }


notificationToast =
    { border = Element.htmlAttribute (Attr.class "notificationToast-border")
    }


notifications =
    { foreground = Element.htmlAttribute (Attr.class "notifications-foreground")
    , background = Element.htmlAttribute (Attr.class "notifications-background")
    , border = Element.htmlAttribute (Attr.class "notifications-border")
    }


notificationLink =
    { foreground = Element.htmlAttribute (Attr.class "notificationLink-foreground")
    }


notificationCenterHeader =
    { foreground = Element.htmlAttribute (Attr.class "notificationCenterHeader-foreground")
    , background = Element.htmlAttribute (Attr.class "notificationCenterHeader-background")
    }


notificationsErrorIcon =
    { foreground = Element.htmlAttribute (Attr.class "notificationsErrorIcon-foreground")
    }


notificationsWarningIcon =
    { foreground = Element.htmlAttribute (Attr.class "notificationsWarningIcon-foreground")
    }


notificationsInfoIcon =
    { foreground = Element.htmlAttribute (Attr.class "notificationsInfoIcon-foreground")
    }


window =
    { activeBorder = Element.htmlAttribute (Attr.class "window-activeBorder")
    , inactiveBorder = Element.htmlAttribute (Attr.class "window-inactiveBorder")
    }


commandCenter =
    { foreground = Element.htmlAttribute (Attr.class "commandCenter-foreground")
    , activeForeground = Element.htmlAttribute (Attr.class "commandCenter-activeForeground")
    , inactiveForeground = Element.htmlAttribute (Attr.class "commandCenter-inactiveForeground")
    , background = Element.htmlAttribute (Attr.class "commandCenter-background")
    , activeBackground = Element.htmlAttribute (Attr.class "commandCenter-activeBackground")
    , border = Element.htmlAttribute (Attr.class "commandCenter-border")
    , activeBorder = Element.htmlAttribute (Attr.class "commandCenter-activeBorder")
    , inactiveBorder = Element.htmlAttribute (Attr.class "commandCenter-inactiveBorder")
    }


editorCommentsWidget =
    { resolvedBorder = Element.htmlAttribute (Attr.class "editorCommentsWidget-resolvedBorder")
    , unresolvedBorder = Element.htmlAttribute (Attr.class "editorCommentsWidget-unresolvedBorder")
    , rangeBackground = Element.htmlAttribute (Attr.class "editorCommentsWidget-rangeBackground")
    , rangeBorder = Element.htmlAttribute (Attr.class "editorCommentsWidget-rangeBorder")
    , rangeActiveBackground = Element.htmlAttribute (Attr.class "editorCommentsWidget-rangeActiveBackground")
    , rangeActiveBorder = Element.htmlAttribute (Attr.class "editorCommentsWidget-rangeActiveBorder")
    }


debugToolBar =
    { background = Element.htmlAttribute (Attr.class "debugToolBar-background")
    , border = Element.htmlAttribute (Attr.class "debugToolBar-border")
    }


debugIcon =
    { startForeground = Element.htmlAttribute (Attr.class "debugIcon-startForeground")
    , breakpointForeground = Element.htmlAttribute (Attr.class "debugIcon-breakpointForeground")
    , breakpointDisabledForeground = Element.htmlAttribute (Attr.class "debugIcon-breakpointDisabledForeground")
    , breakpointUnverifiedForeground = Element.htmlAttribute (Attr.class "debugIcon-breakpointUnverifiedForeground")
    , breakpointCurrentStackframeForeground = Element.htmlAttribute (Attr.class "debugIcon-breakpointCurrentStackframeForeground")
    , breakpointStackframeForeground = Element.htmlAttribute (Attr.class "debugIcon-breakpointStackframeForeground")
    , pauseForeground = Element.htmlAttribute (Attr.class "debugIcon-pauseForeground")
    , stopForeground = Element.htmlAttribute (Attr.class "debugIcon-stopForeground")
    , disconnectForeground = Element.htmlAttribute (Attr.class "debugIcon-disconnectForeground")
    , restartForeground = Element.htmlAttribute (Attr.class "debugIcon-restartForeground")
    , stepOverForeground = Element.htmlAttribute (Attr.class "debugIcon-stepOverForeground")
    , stepIntoForeground = Element.htmlAttribute (Attr.class "debugIcon-stepIntoForeground")
    , stepOutForeground = Element.htmlAttribute (Attr.class "debugIcon-stepOutForeground")
    , continueForeground = Element.htmlAttribute (Attr.class "debugIcon-continueForeground")
    , stepBackForeground = Element.htmlAttribute (Attr.class "debugIcon-stepBackForeground")
    }


mergeEditor =
    { change_background = Element.htmlAttribute (Attr.class "mergeEditor-change-background")
    , change_word_background = Element.htmlAttribute (Attr.class "mergeEditor-change-word-background")
    , changeBase_background = Element.htmlAttribute (Attr.class "mergeEditor-changeBase-background")
    , changeBase_word_background = Element.htmlAttribute (Attr.class "mergeEditor-changeBase-word-background")
    , conflict_unhandledUnfocused_border = Element.htmlAttribute (Attr.class "mergeEditor-conflict-unhandledUnfocused-border")
    , conflict_unhandledFocused_border = Element.htmlAttribute (Attr.class "mergeEditor-conflict-unhandledFocused-border")
    , conflict_handledUnfocused_border = Element.htmlAttribute (Attr.class "mergeEditor-conflict-handledUnfocused-border")
    , conflict_handledFocused_border = Element.htmlAttribute (Attr.class "mergeEditor-conflict-handledFocused-border")
    , conflict_handled_minimapOverViewRuler = Element.htmlAttribute (Attr.class "mergeEditor-conflict-handled-minimapOverViewRuler")
    , conflict_unhandled_minimapOverViewRuler = Element.htmlAttribute (Attr.class "mergeEditor-conflict-unhandled-minimapOverViewRuler")
    , conflictingLines_background = Element.htmlAttribute (Attr.class "mergeEditor-conflictingLines-background")
    , conflict_input1_background = Element.htmlAttribute (Attr.class "mergeEditor-conflict-input1-background")
    , conflict_input2_background = Element.htmlAttribute (Attr.class "mergeEditor-conflict-input2-background")
    }


settings =
    { headerForeground = Element.htmlAttribute (Attr.class "settings-headerForeground")
    , modifiedItemIndicator = Element.htmlAttribute (Attr.class "settings-modifiedItemIndicator")
    , headerBorder = Element.htmlAttribute (Attr.class "settings-headerBorder")
    , sashBorder = Element.htmlAttribute (Attr.class "settings-sashBorder")
    , dropdownBackground = Element.htmlAttribute (Attr.class "settings-dropdownBackground")
    , dropdownForeground = Element.htmlAttribute (Attr.class "settings-dropdownForeground")
    , dropdownBorder = Element.htmlAttribute (Attr.class "settings-dropdownBorder")
    , dropdownListBorder = Element.htmlAttribute (Attr.class "settings-dropdownListBorder")
    , checkboxBackground = Element.htmlAttribute (Attr.class "settings-checkboxBackground")
    , checkboxForeground = Element.htmlAttribute (Attr.class "settings-checkboxForeground")
    , checkboxBorder = Element.htmlAttribute (Attr.class "settings-checkboxBorder")
    , textInputBackground = Element.htmlAttribute (Attr.class "settings-textInputBackground")
    , textInputForeground = Element.htmlAttribute (Attr.class "settings-textInputForeground")
    , textInputBorder = Element.htmlAttribute (Attr.class "settings-textInputBorder")
    , numberInputBackground = Element.htmlAttribute (Attr.class "settings-numberInputBackground")
    , numberInputForeground = Element.htmlAttribute (Attr.class "settings-numberInputForeground")
    , numberInputBorder = Element.htmlAttribute (Attr.class "settings-numberInputBorder")
    , focusedRowBackground = Element.htmlAttribute (Attr.class "settings-focusedRowBackground")
    , rowHoverBackground = Element.htmlAttribute (Attr.class "settings-rowHoverBackground")
    , focusedRowBorder = Element.htmlAttribute (Attr.class "settings-focusedRowBorder")
    }


terminal =
    { background = Element.htmlAttribute (Attr.class "terminal-background")
    , foreground = Element.htmlAttribute (Attr.class "terminal-foreground")
    , selectionBackground = Element.htmlAttribute (Attr.class "terminal-selectionBackground")
    , inactiveSelectionBackground = Element.htmlAttribute (Attr.class "terminal-inactiveSelectionBackground")
    , selectionForeground = Element.htmlAttribute (Attr.class "terminal-selectionForeground")
    , border = Element.htmlAttribute (Attr.class "terminal-border")
    , findMatchBackground = Element.htmlAttribute (Attr.class "terminal-findMatchBackground")
    , findMatchBorder = Element.htmlAttribute (Attr.class "terminal-findMatchBorder")
    , findMatchHighlightBackground = Element.htmlAttribute (Attr.class "terminal-findMatchHighlightBackground")
    , findMatchHighlightBorder = Element.htmlAttribute (Attr.class "terminal-findMatchHighlightBorder")
    , dropBackground = Element.htmlAttribute (Attr.class "terminal-dropBackground")
    , tab_activeBorder = Element.htmlAttribute (Attr.class "terminal-tab-activeBorder")
    , ansiBlack = Element.htmlAttribute (Attr.class "terminal-ansiBlack")
    , ansiRed = Element.htmlAttribute (Attr.class "terminal-ansiRed")
    , ansiGreen = Element.htmlAttribute (Attr.class "terminal-ansiGreen")
    , ansiYellow = Element.htmlAttribute (Attr.class "terminal-ansiYellow")
    , ansiBlue = Element.htmlAttribute (Attr.class "terminal-ansiBlue")
    , ansiMagenta = Element.htmlAttribute (Attr.class "terminal-ansiMagenta")
    , ansiCyan = Element.htmlAttribute (Attr.class "terminal-ansiCyan")
    , ansiWhite = Element.htmlAttribute (Attr.class "terminal-ansiWhite")
    , ansiBrightBlack = Element.htmlAttribute (Attr.class "terminal-ansiBrightBlack")
    , ansiBrightRed = Element.htmlAttribute (Attr.class "terminal-ansiBrightRed")
    , ansiBrightGreen = Element.htmlAttribute (Attr.class "terminal-ansiBrightGreen")
    , ansiBrightYellow = Element.htmlAttribute (Attr.class "terminal-ansiBrightYellow")
    , ansiBrightBlue = Element.htmlAttribute (Attr.class "terminal-ansiBrightBlue")
    , ansiBrightMagenta = Element.htmlAttribute (Attr.class "terminal-ansiBrightMagenta")
    , ansiBrightCyan = Element.htmlAttribute (Attr.class "terminal-ansiBrightCyan")
    , ansiBrightWhite = Element.htmlAttribute (Attr.class "terminal-ansiBrightWhite")
    }


terminalCursor =
    { foreground = Element.htmlAttribute (Attr.class "terminalCursor-foreground")
    , background = Element.htmlAttribute (Attr.class "terminalCursor-background")
    }


terminalCommandDecoration =
    { defaultBackground = Element.htmlAttribute (Attr.class "terminalCommandDecoration-defaultBackground")
    , successBackground = Element.htmlAttribute (Attr.class "terminalCommandDecoration-successBackground")
    , errorBackground = Element.htmlAttribute (Attr.class "terminalCommandDecoration-errorBackground")
    }


terminalOverviewRuler =
    { cursorForeground = Element.htmlAttribute (Attr.class "terminalOverviewRuler-cursorForeground")
    , findMatchForeground = Element.htmlAttribute (Attr.class "terminalOverviewRuler-findMatchForeground")
    }


testing =
    { iconFailed = Element.htmlAttribute (Attr.class "testing-iconFailed")
    , iconErrored = Element.htmlAttribute (Attr.class "testing-iconErrored")
    , iconPassed = Element.htmlAttribute (Attr.class "testing-iconPassed")
    , runAction = Element.htmlAttribute (Attr.class "testing-runAction")
    , iconQueued = Element.htmlAttribute (Attr.class "testing-iconQueued")
    , iconUnset = Element.htmlAttribute (Attr.class "testing-iconUnset")
    , iconSkipped = Element.htmlAttribute (Attr.class "testing-iconSkipped")
    , peekBorder = Element.htmlAttribute (Attr.class "testing-peekBorder")
    , peekHeaderBackground = Element.htmlAttribute (Attr.class "testing-peekHeaderBackground")
    , message_error_decorationForeground = Element.htmlAttribute (Attr.class "testing-message-error-decorationForeground")
    , message_error_lineBackground = Element.htmlAttribute (Attr.class "testing-message-error-lineBackground")
    , message_info_decorationForeground = Element.htmlAttribute (Attr.class "testing-message-info-decorationForeground")
    , message_info_lineBackground = Element.htmlAttribute (Attr.class "testing-message-info-lineBackground")
    }


welcomePage =
    { background = Element.htmlAttribute (Attr.class "welcomePage-background")
    , tileBackground = Element.htmlAttribute (Attr.class "welcomePage-tileBackground")
    , tileHoverBackground = Element.htmlAttribute (Attr.class "welcomePage-tileHoverBackground")
    , tileBorder = Element.htmlAttribute (Attr.class "welcomePage-tileBorder")
    , progress_background = Element.htmlAttribute (Attr.class "welcomePage-progress-background")
    , progress_foreground = Element.htmlAttribute (Attr.class "welcomePage-progress-foreground")
    }


walkthrough =
    { stepTitle_foreground = Element.htmlAttribute (Attr.class "walkthrough-stepTitle-foreground")
    }


walkThrough =
    { embeddedEditorBackground = Element.htmlAttribute (Attr.class "walkThrough-embeddedEditorBackground")
    }


debugExceptionWidget =
    { border = Element.htmlAttribute (Attr.class "debugExceptionWidget-border")
    , background = Element.htmlAttribute (Attr.class "debugExceptionWidget-background")
    }


ports =
    { iconRunningProcessForeground = Element.htmlAttribute (Attr.class "ports-iconRunningProcessForeground")
    }


minimapGutter =
    { modifiedBackground = Element.htmlAttribute (Attr.class "minimapGutter-modifiedBackground")
    , addedBackground = Element.htmlAttribute (Attr.class "minimapGutter-addedBackground")
    , deletedBackground = Element.htmlAttribute (Attr.class "minimapGutter-deletedBackground")
    }


notebook =
    { cellBorderColor = Element.htmlAttribute (Attr.class "notebook-cellBorderColor")
    , focusedEditorBorder = Element.htmlAttribute (Attr.class "notebook-focusedEditorBorder")
    , outputContainerBorderColor = Element.htmlAttribute (Attr.class "notebook-outputContainerBorderColor")
    , outputContainerBackgroundColor = Element.htmlAttribute (Attr.class "notebook-outputContainerBackgroundColor")
    , cellToolbarSeparator = Element.htmlAttribute (Attr.class "notebook-cellToolbarSeparator")
    , focusedCellBackground = Element.htmlAttribute (Attr.class "notebook-focusedCellBackground")
    , selectedCellBackground = Element.htmlAttribute (Attr.class "notebook-selectedCellBackground")
    , cellHoverBackground = Element.htmlAttribute (Attr.class "notebook-cellHoverBackground")
    , selectedCellBorder = Element.htmlAttribute (Attr.class "notebook-selectedCellBorder")
    , inactiveSelectedCellBorder = Element.htmlAttribute (Attr.class "notebook-inactiveSelectedCellBorder")
    , focusedCellBorder = Element.htmlAttribute (Attr.class "notebook-focusedCellBorder")
    , inactiveFocusedCellBorder = Element.htmlAttribute (Attr.class "notebook-inactiveFocusedCellBorder")
    , cellStatusBarItemHoverBackground = Element.htmlAttribute (Attr.class "notebook-cellStatusBarItemHoverBackground")
    , cellInsertionIndicator = Element.htmlAttribute (Attr.class "notebook-cellInsertionIndicator")
    , symbolHighlightBackground = Element.htmlAttribute (Attr.class "notebook-symbolHighlightBackground")
    , cellEditorBackground = Element.htmlAttribute (Attr.class "notebook-cellEditorBackground")
    , editorBackground = Element.htmlAttribute (Attr.class "notebook-editorBackground")
    }


notebookStatusSuccessIcon =
    { foreground = Element.htmlAttribute (Attr.class "notebookStatusSuccessIcon-foreground")
    }


notebookStatusErrorIcon =
    { foreground = Element.htmlAttribute (Attr.class "notebookStatusErrorIcon-foreground")
    }


notebookStatusRunningIcon =
    { foreground = Element.htmlAttribute (Attr.class "notebookStatusRunningIcon-foreground")
    }


notebookScrollbarSlider =
    { background = Element.htmlAttribute (Attr.class "notebookScrollbarSlider-background")
    , hoverBackground = Element.htmlAttribute (Attr.class "notebookScrollbarSlider-hoverBackground")
    , activeBackground = Element.htmlAttribute (Attr.class "notebookScrollbarSlider-activeBackground")
    }


keybindingTable =
    { headerBackground = Element.htmlAttribute (Attr.class "keybindingTable-headerBackground")
    , rowsBackground = Element.htmlAttribute (Attr.class "keybindingTable-rowsBackground")
    }


scm =
    { providerBorder = Element.htmlAttribute (Attr.class "scm-providerBorder")
    }


debugTokenExpression =
    { name = Element.htmlAttribute (Attr.class "debugTokenExpression-name")
    , value = Element.htmlAttribute (Attr.class "debugTokenExpression-value")
    , string = Element.htmlAttribute (Attr.class "debugTokenExpression-string")
    , boolean = Element.htmlAttribute (Attr.class "debugTokenExpression-boolean")
    , number = Element.htmlAttribute (Attr.class "debugTokenExpression-number")
    , error = Element.htmlAttribute (Attr.class "debugTokenExpression-error")
    }


debugView =
    { exceptionLabelForeground = Element.htmlAttribute (Attr.class "debugView-exceptionLabelForeground")
    , exceptionLabelBackground = Element.htmlAttribute (Attr.class "debugView-exceptionLabelBackground")
    , stateLabelForeground = Element.htmlAttribute (Attr.class "debugView-stateLabelForeground")
    , stateLabelBackground = Element.htmlAttribute (Attr.class "debugView-stateLabelBackground")
    , valueChangedHighlight = Element.htmlAttribute (Attr.class "debugView-valueChangedHighlight")
    }


debugConsole =
    { infoForeground = Element.htmlAttribute (Attr.class "debugConsole-infoForeground")
    , warningForeground = Element.htmlAttribute (Attr.class "debugConsole-warningForeground")
    , errorForeground = Element.htmlAttribute (Attr.class "debugConsole-errorForeground")
    , sourceForeground = Element.htmlAttribute (Attr.class "debugConsole-sourceForeground")
    }


debugConsoleInputIcon =
    { foreground = Element.htmlAttribute (Attr.class "debugConsoleInputIcon-foreground")
    }


extensionButton =
    { background = Element.htmlAttribute (Attr.class "extensionButton-background")
    , foreground = Element.htmlAttribute (Attr.class "extensionButton-foreground")
    , hoverBackground = Element.htmlAttribute (Attr.class "extensionButton-hoverBackground")
    , separator = Element.htmlAttribute (Attr.class "extensionButton-separator")
    , prominentBackground = Element.htmlAttribute (Attr.class "extensionButton-prominentBackground")
    , prominentForeground = Element.htmlAttribute (Attr.class "extensionButton-prominentForeground")
    , prominentHoverBackground = Element.htmlAttribute (Attr.class "extensionButton-prominentHoverBackground")
    }


extensionIcon =
    { starForeground = Element.htmlAttribute (Attr.class "extensionIcon-starForeground")
    , verifiedForeground = Element.htmlAttribute (Attr.class "extensionIcon-verifiedForeground")
    , preReleaseForeground = Element.htmlAttribute (Attr.class "extensionIcon-preReleaseForeground")
    , sponsorForeground = Element.htmlAttribute (Attr.class "extensionIcon-sponsorForeground")
    }


interactive =
    { activeCodeBorder = Element.htmlAttribute (Attr.class "interactive-activeCodeBorder")
    , inactiveCodeBorder = Element.htmlAttribute (Attr.class "interactive-inactiveCodeBorder")
    }


gitDecoration =
    { addedResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-addedResourceForeground")
    , modifiedResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-modifiedResourceForeground")
    , deletedResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-deletedResourceForeground")
    , renamedResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-renamedResourceForeground")
    , untrackedResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-untrackedResourceForeground")
    , ignoredResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-ignoredResourceForeground")
    , stageModifiedResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-stageModifiedResourceForeground")
    , stageDeletedResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-stageDeletedResourceForeground")
    , conflictingResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-conflictingResourceForeground")
    , submoduleResourceForeground = Element.htmlAttribute (Attr.class "gitDecoration-submoduleResourceForeground")
    }


gitlens =
    { gutterBackgroundColor = Element.htmlAttribute (Attr.class "gitlens-gutterBackgroundColor")
    , gutterForegroundColor = Element.htmlAttribute (Attr.class "gitlens-gutterForegroundColor")
    , gutterUncommittedForegroundColor = Element.htmlAttribute (Attr.class "gitlens-gutterUncommittedForegroundColor")
    , trailingLineBackgroundColor = Element.htmlAttribute (Attr.class "gitlens-trailingLineBackgroundColor")
    , trailingLineForegroundColor = Element.htmlAttribute (Attr.class "gitlens-trailingLineForegroundColor")
    , lineHighlightBackgroundColor = Element.htmlAttribute (Attr.class "gitlens-lineHighlightBackgroundColor")
    , lineHighlightOverviewRulerColor = Element.htmlAttribute (Attr.class "gitlens-lineHighlightOverviewRulerColor")
    , openAutolinkedIssueIconColor = Element.htmlAttribute (Attr.class "gitlens-openAutolinkedIssueIconColor")
    , closedAutolinkedIssueIconColor = Element.htmlAttribute (Attr.class "gitlens-closedAutolinkedIssueIconColor")
    , closedPullRequestIconColor = Element.htmlAttribute (Attr.class "gitlens-closedPullRequestIconColor")
    , openPullRequestIconColor = Element.htmlAttribute (Attr.class "gitlens-openPullRequestIconColor")
    , mergedPullRequestIconColor = Element.htmlAttribute (Attr.class "gitlens-mergedPullRequestIconColor")
    , unpublishedChangesIconColor = Element.htmlAttribute (Attr.class "gitlens-unpublishedChangesIconColor")
    , unpublishedCommitIconColor = Element.htmlAttribute (Attr.class "gitlens-unpublishedCommitIconColor")
    , unpulledChangesIconColor = Element.htmlAttribute (Attr.class "gitlens-unpulledChangesIconColor")
    , decorations_addedForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-addedForegroundColor")
    , decorations_copiedForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-copiedForegroundColor")
    , decorations_deletedForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-deletedForegroundColor")
    , decorations_ignoredForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-ignoredForegroundColor")
    , decorations_modifiedForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-modifiedForegroundColor")
    , decorations_untrackedForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-untrackedForegroundColor")
    , decorations_renamedForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-renamedForegroundColor")
    , decorations_branchAheadForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-branchAheadForegroundColor")
    , decorations_branchBehindForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-branchBehindForegroundColor")
    , decorations_branchDivergedForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-branchDivergedForegroundColor")
    , decorations_branchUpToDateForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-branchUpToDateForegroundColor")
    , decorations_branchUnpublishedForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-branchUnpublishedForegroundColor")
    , decorations_branchMissingUpstreamForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-branchMissingUpstreamForegroundColor")
    , decorations_worktreeView_hasUncommittedChangesForegroundColor = Element.htmlAttribute (Attr.class "gitlens-decorations-worktreeView-hasUncommittedChangesForegroundColor")
    , graphLane1Color = Element.htmlAttribute (Attr.class "gitlens-graphLane1Color")
    , graphLane2Color = Element.htmlAttribute (Attr.class "gitlens-graphLane2Color")
    , graphLane3Color = Element.htmlAttribute (Attr.class "gitlens-graphLane3Color")
    , graphLane4Color = Element.htmlAttribute (Attr.class "gitlens-graphLane4Color")
    , graphLane5Color = Element.htmlAttribute (Attr.class "gitlens-graphLane5Color")
    , graphLane6Color = Element.htmlAttribute (Attr.class "gitlens-graphLane6Color")
    , graphLane7Color = Element.htmlAttribute (Attr.class "gitlens-graphLane7Color")
    , graphLane8Color = Element.htmlAttribute (Attr.class "gitlens-graphLane8Color")
    , graphLane9Color = Element.htmlAttribute (Attr.class "gitlens-graphLane9Color")
    , graphLane10Color = Element.htmlAttribute (Attr.class "gitlens-graphLane10Color")
    }
