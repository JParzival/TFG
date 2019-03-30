'use strict'

const mongoose = require('mongoose')
const server = require('./server')

const urlServer = 'mongodb://127.0.0.1:27017/tfg'
const port = process.env.port || 9797

mongoose.connect(urlServer, {useNewUrlParser: true})
    .then(() => {
        console.log("Conectado a la base de datos con éxito!")
        const servidor = server.listen(port, () => 
        {
            console.log(`Servidor escuchando en ${servidor.address().port}`)
        })
    })
    .catch(err => console.log(err))
