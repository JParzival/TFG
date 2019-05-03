'use strict'

const Paciente = require("../modelos/paciente")

function obtenerPacientes(req, res)
{
    Paciente.find({}).exec((err, listadoPacientes) => {
        if(err)
        {
            res.status(500).send(
                {
                    message: "Error al recuperar los pacientes"
                }
            )
        }
        else
        {
            if(listadoPacientes)
            {
                res.status(200).send({listadoPacientes})
            }
            else
            {
                res.status(404).send(
                    {
                        message: "No se ha podido recuperar el listado de pacientes"
                    }
                )
            }
        }
    })
}

function obtenerPaciente(req, res)
{
    Paciente.find({'_id': req.params.idPaciente}).exec((err, paciente) => {
        if(err)
        {
            res.status(500).send(
                {
                    message: "Error al recuperar el paciente"
                }
            )
        }
        else
        {
            if(Paciente)
            {
                res.status(200).send({paciente})
            }
            else
            {
                res.status(404).send(
                    {
                        message: "No se ha podido recuperar el paciente"
                    }
                )
            }
        }
    })
}

function borrarPaciente(req, res) 
{
    Paciente.findOneAndRemove({'_id':req.params.idPaciente},
                          (err, pacienteEliminado) =>
                          {
                            if (err) 
                            {
                                res.status(500).send(
                                    {
                                        message: "Error al eliminar el paciente"
                                    }
                                )
                            }
                            else
                            {
                                if(pacienteEliminado)
                                {
                                    res.status(200).send(
                                        {
                                            paciente: pacienteEliminado
                                        }
                                    )
                                }
                                else
                                {
                                    res.status(404).send(
                                        {
                                            message: "No existe el paciente a eliminar"
                                        }
                                    )
                                }
                                
                            } 
                          })
}

module.exports = 
{ 
    obtenerPaciente,
    obtenerPacientes,
    borrarPaciente
}