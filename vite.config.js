import { resolve } from 'path'
import { defineConfig } from 'vite'
import { plugin } from 'vite-plugin-elm'

export default defineConfig({
    plugins: [plugin({debug: false})]
})

