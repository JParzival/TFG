'use strict'

const mongoose = require('mongoose')
const Schema = mongoose.Schema

const PacienteSchema = new Schema({

    //no se necesita el identificador porque lo crea mongo

    nom: String,
    edad: Number,
    sex: Number,
    rel_ctxo_rel_mala: Number,
    rel_ctxo_trauma: Number,
    rel_ctxo_buena: Number,
    ed_perm: Number,
    ed_normal: Number,
    ed_estr: Number,
    resil_ba: Number,
    resil_me: Number,
    resil_al: Number,
    pen_dic: Number,
    gen_ex: Number,
    etiq: Number,
    fil_men: Number,
    max_min: Number,
    conc_arb: Number,
    pseu_res: Number,
    deb: Number,
    raz_emo: Number,
    inhib: Number,
    asert: Number,
    agres: Number,
    impuls: Number,
    grupo: Number

})

module.exports = mongoose.model('Paciente', PacienteSchema)