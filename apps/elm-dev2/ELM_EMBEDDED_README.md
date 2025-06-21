# Elm-embedded Custom Element

The `elm-embedded` custom element allows you to embed compiled Elm applications from an elm-dev server into any web page.

## Usage

### In Elm

```elm
import WebComponents.Elm

-- Create an elm-embedded element
WebComponents.Elm.elm
    { baseUrl = "http://localhost:51213"
    , filepath = "path/to/your/elm/file.elm"
    }
```

### In HTML

```html
<elm-embedded 
    base-url="http://localhost:51213" 
    filepath="path/to/your/elm/file.elm">
</elm-embedded>
```

### In JavaScript

```javascript
const element = document.createElement('elm-embedded');
element.setAttribute('base-url', 'http://localhost:51213');
element.setAttribute('filepath', 'path/to/your/elm/file.elm');
document.body.appendChild(element);
```

## Properties

- `base-url` (required): The base URL of the elm-dev server
- `filepath` (required): The path to the Elm file that should be compiled and embedded

## How it works

1. The custom element fetches compiled JavaScript from the elm-dev server at `{base-url}/interactive?file={filepath}`
2. The compiled code is executed to define the Elm object
3. The Elm app is initialized and mounted to a container within the custom element
4. The app runs in a shadow DOM for encapsulation

## Requirements

- The Elm file must have a `main` function that returns a `Program`
- The elm-dev server must be running and accessible
- The filepath must be relative to the elm-dev server's project root

## Error Handling

If the custom element fails to load or mount the Elm app, it will display an error message with details about what went wrong.

## Example

See `example/src/Test/Embedded.elm` for a complete example of an Elm file that can be embedded.

## Implementation Details

- **Elm side**: `apps/elm-dev2/src/app/WebComponents/Elm.elm`
- **TypeScript side**: `apps/elm-dev2/src/js/webcomponents/elm-embedded.ts`
- **Registration**: `apps/elm-dev2/src/js/webcomponents/index.ts` 