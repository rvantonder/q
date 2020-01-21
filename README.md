# README

## Very long query?

```
echo 'const p = require("./parser.min.js"); console.log(p.parse("very long query"))' | node --stack-size=9999
```
