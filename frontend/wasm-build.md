Hi, I just need some clarification. Here's the project outline I have been working on

### Dir Structure

```text
.
├── .gitignore
├── Cargo.toml
├── Cargo.lock
├── doc
├── example-programs
├── tests
├── src
│   │   ├── bin
│   │   └── main.rs
│   ├── codegen.rs
│   ├── dfa
│   │   └── parser.rs
│   ├── dfa.rs
│   ├── enumerate.rs
│   ├── eval.rs
│   ├── lib.rs
│   ├── parse.rs
│   ├── syntax.rs
│   └── typecheck.rs
└── frontend
    ├── index.html                        // Entry point (loads main.tsx)
    ├── node_modules/..
    ├── package-lock.json
    ├── package.json
    ├── postcss.config.js
    ├── public
    │   ├── favicon.png
    │   └── robots.txt
    ├── README.md
    ├── src
    │   ├── App.tsx                       // ties editor and viewer together
    │   ├── components
    │   │   ├── AutomataEditor.tsx        // .un file editor
    │   │   ├── EditorTabs.tsx            // combines `un` code and mermaid code into tabs in the same pane
    │   │   ├── Footer.tsx
    │   │   ├── Header.tsx
    │   │   ├── MermaidView.tsx           // mermaid viewer
    │   │   ├── MermaidEditor.tsx
    │   │   └── ThemeToggle.tsx
    │   ├── contexts
    │   │   └── ThemeContext.tsx
    │   ├── index.css
    │   ├── main.tsx                      // loads App.tsx
    │   ├── services
    │   │   └── AutomataBridge.ts
    │   └── wasm
    │       └── automata.ts
    ├── tailwind.config.js
    ├── tsconfig.json
    ├── tsconfig.node.json
    └── vite.config.ts
```

I created the webui with React and Vite. I really like the setup as of now. But having a hard time wrapping my head around the data flow from the `AutomataEditor.tsx` to `MermaidView.tsx`. So please correct me if something is the wrong approach.

### I am planning the following

1. Rust → Wasm: expose an entry point for `dfa.rs`

    ```rs
    // src/dfa.rs

    /// DFA states.  Never create a state object yourself, always use
    /// `State::fresh`.
    #[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
    pub struct State(u32);

    /// A deterministic finite automaton.
    #[derive(Debug)]
    pub struct Dfa<Symbol: Hash + Eq> {
        /// The set of all possible states
        pub states: Set<State>,
        /// The set of all possible symbols
        pub alphabet: Set<Symbol>,
        /// Curried transition function
        pub trans: Map<State, Map<Symbol, State>>,
        /// Start state
        pub start: State,
        /// Accepting states
        pub accepting: Set<State>,
        /// State names, optional
        pub state_names: Map<State, String>,
    }

    impl std::fmt::Display for Dfa {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            // ... write mermaid `stateDiagram-v2` text into `f` ...
        }
    }
    ```

    Add a little wrapper in `wasm.rs` (the crate to build with wasm-pack):

    ```rs
    use wasm_bindgen::prelude::*;
    use uncrustable::parse::parse;
    use uncrustable::parser::parse;
    use uncrustable::dfa::Dfa;

    #[wasm_bindgen]
    pub fn render_mermaid(input: &str) -> Result<String, JsValue> {
        let program = parse::parse(input).unwrap();

        let dfa = enumerate::enumerate(program).unwrap()
            .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;

        // `to_string()` uses your Display impl
        Ok(dfa.to_string())
    }
    ```

    Build wasm-pack

    ```zsh
    wasm-pack build --target bundler --out-dir ../web/src/wasm
    ```

    This creates a `pkg/` folder under `src/wasm` with `automata.ts`, `automata_bg.wasm`, etc.

2. Add the imports in Vite, then I could access the structs and functions from `dfa.rs`

    ```ts
    // src/wasm/automata.ts
    import init, { render_mermaid as _renderMermaid } from './automata/pkg/automata';

    let ready: Promise<void>;
    export function initWasm() {
    if (!ready) ready = init();
    return ready;
    }

    export async function renderMermaid(src: string): Promise<string> {
    await initWasm();
    // this will either return the mermaid text or throw an error
    return _renderMermaid(src);
    }
    ```

3. Then I can access the functions like this

    ```rs
    // src/services/AutomataBridge.ts
    import { renderMermaid } from '../wasm/automata';

    export function parseDfa(src: string): Promise<string> {
    return renderMermaid(src);
    }
    ```

4. Hook it into React

    In App.tsx’s effect:

    ```tsx
    useEffect(() => {
    const tid = setTimeout(() => {
        parseDfa(code)
        .then(setDiagram)
        .catch(err => setDiagram(`\`\`\`\n${err}\n\`\`\``))
    }, 300);
    return () => clearTimeout(tid);
    }, [code]);
    ```

    And `<Mermaid chart={diagram} />` should render exactly what the Rust Display produced.

---

### Just to recap

**Rust**: We already wrote the pretty‐printer via Display.

**Wasm**: wasm_bindgen converts between Rust String and JS strings.

**TS Bridge**: just forwards the call, no JSON gymnastics needed.

**React**: debounce and feed the raw Mermaid text straight into the `<Mermaid>` component.

Does this work? Please let me know if something needs modification.