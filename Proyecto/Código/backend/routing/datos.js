'use strict'

const express = require('express');

const datosControlador = require('../controladores/datos');

let api = express.Router();

api.get('/datos', datosControlador.obtenerPacientes);

api.get('/datos/:idPaciente', datosControlador.obtenerPaciente);

api.delete('/datos/:idPaciente', datosControlador.borrarPaciente);

module.exports = api