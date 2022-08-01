import { resolve } from 'path'
import { defineConfig } from 'vite'
import Elm from 'vite-plugin-elm'
import StringReplace from 'vite-plugin-string-replace'

export default defineConfig({
    plugins: [
        Elm({debug: false}),
        StringReplace([
            { search: /(\sA\d\([\s]+)([^,]+)(,[\s]+)(function[^(]+)\(/gm, replace:`$1$2$3$4___$2(` },
            { search: /var ([^=]+)( = F\d\([^f]+function)[^(]\(/gmi, replace: `var $1$2 __$1( `}
        ])

    ]
})

