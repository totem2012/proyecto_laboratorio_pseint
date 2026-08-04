// =====================================================================
// FIXLY - Sistema de gestion de taller mecanico
//
// Cada uno escribe SOLO dentro de la opcion que le toca, entre las
// lineas que marcan el inicio y el fin de su modulo. No tocar el resto
// del archivo: si hace falta un cambio afuera, se avisa al grupo.
// =====================================================================

Algoritmo Fixly

	Definir opcion Como Entero
	Definir tecla Como Caracter

	Repetir
		Limpiar Pantalla
		Escribir "============================================"
		Escribir "        FIXLY - Gestion de taller"
		Escribir "============================================"
		Escribir "  1 - Clientes"
		Escribir "  2 - Vehiculos"
		Escribir "  3 - Ordenes de trabajo"
		Escribir "  4 - Repuestos"
		Escribir "  5 - Facturacion"
		Escribir "  6 - Historial de reparaciones"
		Escribir "  7 - Usuarios"
		Escribir "  0 - Salir"
		Escribir "============================================"
		Escribir Sin Saltar "Opcion: "
		Leer opcion

		Segun opcion Hacer
			1:
				// ------------------------------------------------
				// MODULO CLIENTES - INICIO
				// ------------------------------------------------
				Escribir "Clientes: en construccion"
				Escribir Sin Saltar "Presione ENTER para continuar..."
				Leer tecla
				// ------------------------------------------------
				// MODULO CLIENTES - FIN
				// ------------------------------------------------

			2:
				// ------------------------------------------------
				// MODULO VEHICULOS - INICIO
				// ------------------------------------------------
				Escribir "Vehiculos: en construccion"
				Escribir Sin Saltar "Presione ENTER para continuar..."
				Leer tecla
				// ------------------------------------------------
				// MODULO VEHICULOS - FIN
				// ------------------------------------------------

			3:
				// ------------------------------------------------
				// MODULO ORDENES DE TRABAJO - INICIO
				// ------------------------------------------------
				Escribir "Ordenes de trabajo: en construccion"
				Escribir Sin Saltar "Presione ENTER para continuar..."
				Leer tecla
				// ------------------------------------------------
				// MODULO ORDENES DE TRABAJO - FIN
				// ------------------------------------------------

			4:
				// ------------------------------------------------
				// MODULO REPUESTOS - INICIO
				// ------------------------------------------------
				Escribir "Repuestos: en construccion"
				Escribir Sin Saltar "Presione ENTER para continuar..."
				Leer tecla
				// ------------------------------------------------
				// MODULO REPUESTOS - FIN
				// ------------------------------------------------

			5:
				// ------------------------------------------------
				// MODULO FACTURACION - INICIO
				// ------------------------------------------------
				Escribir "Facturacion: en construccion"
				Escribir Sin Saltar "Presione ENTER para continuar..."
				Leer tecla
				// ------------------------------------------------
				// MODULO FACTURACION - FIN
				// ------------------------------------------------

			6:
				// ------------------------------------------------
				// MODULO HISTORIAL DE REPARACIONES - INICIO
				// ------------------------------------------------
				Escribir "Historial de reparaciones: en construccion"
				Escribir Sin Saltar "Presione ENTER para continuar..."
				Leer tecla
				// ------------------------------------------------
				// MODULO HISTORIAL DE REPARACIONES - FIN
				// ------------------------------------------------

			7:
				// ------------------------------------------------
				// MODULO USUARIOS - INICIO
				// ------------------------------------------------
				Escribir "Usuarios: en construccion"
				Escribir Sin Saltar "Presione ENTER para continuar..."
				Leer tecla
				// ------------------------------------------------
				// MODULO USUARIOS - FIN
				// ------------------------------------------------

			0:
				Escribir "Hasta luego."

			De Otro Modo:
				Escribir "Opcion no valida."
				Escribir Sin Saltar "Presione ENTER para continuar..."
				Leer tecla
		FinSegun

	Hasta Que opcion = 0

FinAlgoritmo
