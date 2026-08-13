
// =====================================================================
// FIXLY - Sistema de gestion de taller mecanico
//
// Cada uno escribe SOLO dentro de la opcion que le toca, entre las
// lineas que marcan el inicio y el fin de su modulo. No tocar el resto
// del archivo: si hace falta un cambio afuera, se avisa al grupo.
// =====================================================================

Algoritmo Fixly

	Definir opcion, stock, cantidad_repuestos, i, opcion_repuestos, posicion_encontrada, j Como Entero
	Definir codigo, nombre, nombre_buscar, codigo_buscar, tecla Como Caracter
	Definir precio, precio_nuevo Como Real
	Definir existe Como Logico
	Dimension codigo[100]
	Dimension nombre[100]
	Dimension precio[100]
	Dimension stock[100]

	cantidad_repuestos <- 0

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
			
				
				Repetir
					Limpiar Pantalla
					Escribir "=================================="
					Escribir "	  Módulo gestión de repuestos"
					Escribir "=================================="
					Escribir "  1 - Cargar stock repuesto"
					Escribir "  2 - Eliminar repuesto"
					Escribir "  3 - Código de repuesto"
					Escribir "  4 - Modificar precio"
					Escribir "  5 - Ver listado de repuestos"
					Escribir "  0 - Volver al menú principal"
					Leer opcion_repuestos
					Segun opcion_repuestos Hacer
						
						1:
							Si cantidad_repuestos < 100 Entonces
								Escribir "Cargar respuesto:"
								Escribir Sin Saltar "Código repuesto:"
								Leer codigo[cantidad_repuestos]
								Escribir Sin Saltar "Nombre repuesto:"
								Leer nombre[cantidad_repuestos]
								Escribir Sin Saltar "Precio repuesto:"
								Leer precio[cantidad_repuestos]
								Escribir "Stock repuesto:"
								Leer stock[cantidad_repuestos]
								cantidad_repuestos <- cantidad_repuestos + 1
								Escribir "Cantidad de repuestos: ", cantidad_repuestos
								Escribir Sin Saltar "Presione ENTER para continuar..."
								Leer tecla
								Escribir "Código guardado: ", codigo[cantidad_repuestos - 1]
								Escribir "Nombre guardado: ", nombre[cantidad_repuestos - 1]
								Escribir Sin Saltar "Presione ENTER para continuar..."
								Leer tecla
							SiNo
								Escribir "Stock completo"
								
							FinSi
						2:							
							Escribir "Seleccionar código para eliminar"
							Leer codigo_buscar
							posicion_encontrada <- -1
							Para i <- 0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
								Si codigo_buscar = codigo[i] Entonces
									posicion_encontrada <- i
								
								FinSi
							FinPara
							
							Si posicion_encontrada <> -1 Entonces
								Para j <- posicion_encontrada Hasta cantidad_repuestos - 2 Con Paso 1 Hacer
									codigo[j] <- codigo[j+1]
									nombre[j] <- nombre[j+1]
									precio[j] <- precio[j+1]
									stock[j] <- stock[j+1]
									
								FinPara
								Escribir "Repuesto eliminado con éxito"
								cantidad_repuestos <- cantidad_repuestos - 1
								
								
							SiNo	
								Escribir "El código no existe"
								
							FinSi
							Escribir Sin Saltar "Presione ENTER para continuar..."
							Leer tecla
						3: 
							Escribir "Ingrese el nombre del repuesto: "
							Leer nombre_buscar
							posicion_encontrada <- -1
							
							Para i <- 0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
								Si nombre_buscar = nombre[i] Entonces
									posicion_encontrada <- i
								FinSi
							FinPara
							
							Si posicion_encontrada <> -1 Entonces
								Escribir " ", codigo[posicion_encontrada]
								Escribir " ",nombre[posicion_encontrada] 
								Escribir " ",precio[posicion_encontrada] 
								Escribir " ",stock[posicion_encontrada] 
							SiNo
								Escribir "Repuesto no encontrado"
							FinSi
							Escribir Sin Saltar "Presione ENTER para continuar..."
							Leer tecla
						4:
							Escribir "Ingrese el nombre del repuesto: "
							Leer nombre_buscar
							posicion_encontrada <- -1
							
							Para i <- 0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
								Si nombre_buscar = nombre[i] Entonces
									posicion_encontrada <- i
								FinSi
							FinPara
							
							Si posicion_encontrada <> -1 Entonces
								Escribir "Ingrese el precio nuevo: "
								Leer precio_nuevo
								precio[posicion_encontrada] <- precio_nuevo
								Escribir "Precio actualizado exitosamente"
							SiNo
								Escribir "Repuesto no encontrado"
							FinSi
							Escribir Sin Saltar "Presione ENTER para continuar..."
							Leer tecla
							
						5:
							Escribir "Listado de repuestos"
							Escribir "=================================="
							Si cantidad_repuestos = 0 Entonces
								Escribir "No hay repuestos cargados"
							SiNo
								Para i <- 0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
									Escribir "Repuesto ", i + 1, ":"
									Escribir "  Código: ", codigo[i]
									Escribir "  Nombre: ", nombre[i]
									Escribir "  Precio: $", precio[i]
									Escribir "  Stock: ", stock[i]
									Escribir "----------------------------------"
								FinPara
							FinSi
							Escribir Sin Saltar "Presione ENTER para continuar..."
							Leer tecla
					FinSegun
					
				Mientras Que opcion_repuestos <> 0

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
