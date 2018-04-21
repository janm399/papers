const express = require('express')
const bodyParser = require('body-parser')
const rawBodyParser = bodyParser.raw({ type: 'application/octet-stream' });
const app = express()

app.post('/schedule', rawBodyParser, (req, res) => res.send('done ' + req.body.length))

app.listen(3000, () => console.log('Example app listening on port 3000!'))