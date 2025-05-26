
export const toJs = (id, wasSuccessful) => `
if (import.meta.hot) {
  let id = "${id}"
  let wasSuccessful = ${wasSuccessful}

  class ElmErrorOverlay extends HTMLElement {
    constructor() {
      super()
      this.attachShadow({ mode: 'open' })
    }

    onContentChanged(html) {
      this.shadowRoot.querySelector('.elm-error').innerHTML = html

      // Adds "Jump to problem" button click listeners
      let buttons = this.shadowRoot.querySelectorAll('button[data-source]')
      for (let btn of buttons) {
        btn.addEventListener('click', () => {
          let filepath = btn.getAttribute('data-source')
          import.meta.hot.send('elm:open-editor', { filepath })
        })
      }
    }

    connectedCallback() {
      this.shadowRoot.innerHTML = \`
        <style>
          /* ATOM DARK */
          :host {
            --elmError__red: #E06C75;
            --elmError__green: #98C379;
            --elmError__yellow: #E5C07B;
            --elmError__blue: #61AFEF;
            --elmError__magenta: #C678DD;
            --elmError__cyan: #56B6C2;
            --elmError__background: #282C34;
            --elmError__foreground: #e6e7eb;
          }
      
          @media (prefers-color-scheme: light) {
            /* ATOM LIGHT */
            :host {
              --elmError__red: #CA1243;
              --elmError__green: #50A14F;
              --elmError__yellow: #C18401;
              --elmError__blue: #4078F2;
              --elmError__magenta: #A626A4;
              --elmError__cyan: #0184BC;
              --elmError__background: #FAFAFA;
              --elmError__foreground: #383A42;
            }
          }

          elm-error-overlay {
            display: flex;
            align-items: center;
            justify-content: center;
            position: fixed;
            z-index: 9669;
          }
          button {
            font-size: inherit;
            padding: 0;
            color: inherit;
            background: none;
            border: 0;
            font-family: inherit;
            cursor: pointer;
            position: relative;
            z-index: 1;
            margin: 0;
            border-bottom: solid 0.1em;
          }
          a:hover, button:hover {
            opacity: 0.75;
          }
          button:active {
            opacity: 1;
            color: var(--elmError__foreground)
          }
          .elm-error__background {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            opacity: 0.25;
            background: #000;
          }
          .elm-error__parent {    
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            display: flex;
            align-items: center;
            justify-content: center;
            padding: 1em;
            font-family: 'Fira Code', Consolas, "Andale Mono WT", "Andale Mono", "Lucida Console", "Lucida Sans Typewriter", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", "Liberation Mono", "Nimbus Mono L", Monaco, "Courier New", Courier, monospace;
            font-variant-ligatures: none;
            font-weight: 400;
            font-size: clamp(0.5rem, 2vw, 1rem);
          }
          .elm-error {
            position: relative;
            background: var(--elmError__background);
            color: var(--elmError__foreground);
            white-space: nowrap;
            line-height: 1.5;
            border-radius: 0.5em;
            box-shadow: 0 1em 1em rgba(0, 0, 0, 0.125);
            border-top: solid 0.5em var(--elmError__red);
            max-height: calc(100vh - 4em);
            overflow: auto;
            max-width: 100%;
            box-sizing: border-box;
          }
          .elm-error > * {
            max-width: calc(80em - 4em);
            padding: 2em;
          }
        </style>
        <div class="elm-error__background"></div>
        <div class="elm-error__parent">
          <div class="elm-error"></div>
        </div>
      \`
    }
  }

  import.meta.hot.on('elm:error', (data) => {
    if (!customElements.get('elm-error-overlay')) {
      customElements.define('elm-error-overlay', ElmErrorOverlay)
    }

    let existingOverlay = document.querySelector('elm-error-overlay')
    if (existingOverlay) {
      existingOverlay.onContentChanged(data.error)
    } else {
      let node = document.createElement('elm-error-overlay')
      let root = document.body
      root.appendChild(node)
      document.querySelector('elm-error-overlay').onContentChanged(data.error)
    }
  })

  import.meta.hot.on('elm:success', (data) => {
    if (data.id === id) {
      let existingOverlay = document.querySelector('elm-error-overlay')
      if (existingOverlay) {
        existingOverlay.remove()
      }
    }
    if (!wasSuccessful) {
      import.meta.hot.invalidate('Triggering reload!')
    }
  })

  if (import.meta.env.DEV) {
    import.meta.hot.send('elm:client-ready', { id })
  }

  import.meta.hot.accept((module) => {
    console.log('Hot accept', module);
  })

 
}
`

const hotLoader = `import.meta.hot.accept((module) => {
    var data = module.default.init("__elmWatchReturnData");
    var apps = import.meta.hot.data.elmApps || [];
    var reloadReasons = [];
    for (var index = 0; index < apps.length; index++) {
      var app = apps[index];
      if (app.__elmWatchProgramType !== data.programType) {
        reloadReasons.push({
          tag: "ProgramTypeChanged",
          previousProgramType: app.__elmWatchProgramType,
          newProgramType: data.programType,
        });
      } else {
        try {
          var innerReasons = app.__elmWatchHotReload(data);
          reloadReasons = reloadReasons.concat(innerReasons);
        } catch (error) {
          reloadReasons.push({
            tag: "HotReloadCaughtError",
            caughtError: error,
          });
        }
      }
    }
    if (reloadReasons.length > 0) {
      for (var index = 0; index < apps.length; index++) {
        let app = apps[index]
        if (app && app.unmount) { app.unmount() }
      }
      import.meta.hot.invalidate(reloadReasons[0].tag);
    }
  })`