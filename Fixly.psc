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
	
	// =====================================================================
	// FIXLY - Variable de opcion vehiculo//
	Definir menuVehiculo, contadorVehiculo, dniDueno, i Como Entero
	Definir marcaVehiculo, modeloVehiculo, patente Como Caracter
	Dimensionar marcaVehiculo[50], modeloVehiculo[50], patente[50], dniDueno[50]
	contadorVehiculo<-0
	// =====================================================================
	// --- Carga de datos de prueba ---
	contadorVehiculo <- 5
	
	marcaVehiculo[0] <- "Ford"
	modeloVehiculo[0] <- "Fiesta"
	patente[0] <- "AB123CD"
	dniDueno[0] <- 30111222
	
	marcaVehiculo[1] <- "Chevrolet"
	modeloVehiculo[1] <- "Onix"
	patente[1] <- "AC456EF"
	dniDueno[1] <- 28555666
	
	marcaVehiculo[2] <- "Toyota"
	modeloVehiculo[2] <- "Corolla"
	patente[2] <- "AD789GH"
	dniDueno[2] <- 35222111
	
	marcaVehiculo[3] <- "Renault"
	modeloVehiculo[3] <- Sandero
	patente[3] <- "AE012IJ"
	dniDueno[3] <- 40333444
	
	marcaVehiculo[4] <- "Volkswagen"
	modeloVehiculo[4] <- "Gol"
	patente[4] <- "AF345KL"
	dniDueno[4] <- 27888999
	// --- Fin carga de datos de prueba ---
	
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
				Si contadorVehiculo<51 Entonces
					
					Escribir "Vehiculos: en construccion"
					Escribir "Ingrese una de las siguientes opciones: "
					Escribir "  1 - Ingresar un nuevo vehiculo"
					Escribir "  2 - Ver lista de vehiculos"
					Escribir "  3 - Quitar un vehiculo"
					Escribir "  0 - Volver"
					Leer menuVehiculo
					Segun menuVehiculo Hacer
						1:
							contadorVehiculo<-contadorVehiculo+1
							Escribir "Ingrese los siguientes datos de vehiculos"
							Escribir "Marca del vehiculo"
							Leer marcaVehiculo[contadorVehiculo]
							Escribir "Modelo del vehiculo"
							Leer modeloVehiculo[contadorVehiculo]
							Escribir "Patente del vehiculo"
							Leer patente[contadorVehiculo]
							Repetir
								Escribir "DNI dueño del vehiculo (Sin puntos)"
								Leer dniDueno[contadorVehiculo]
								Si (dniDueno[contadorVehiculo]<0) o (dniDueno[contadorVehiculo]>100000000) Entonces
									Escribir "Valor de DNI invalido"
								FinSi
							Mientras Que dniDueno[contadorVehiculo]>0 y dniDueno[contadorVehiculo]<100000000
							
						2:
							Escribir "MARCA", "		", "MODELO", "		", "PATENTE", "		", "DNI DUEÑO"
							Escribir "---------------------------------------------"
							Para i<-0 Hasta contadorVehiculo-1 Con Paso 1 Hacer
								Escribir marcaVehiculo[i], Tab, modeloVehiculo[i], Tab, patente[i], Tab, dniDueno[i]
							FinPara
						3:
							Escribir Sin Saltar "Escriba la patente del vehiculo que desea eliminar"
							Leer patenteEliminar
							Para i<-0 Hasta contadorVehiculo-1 Con Paso 1 Hacer
								si patente[i] = patenteEliminar Entonces
									marcaVehiculo[i]<-0
									modeloVehiculo[i]<-0
									patente[i]<-0
									dniDueno[i]<-0
									lugarVacio<-i
									Para i<-lugarvacio Hasta contadorVehiculo-1 Con Paso paso Hacer
										marcaVehiculo[i]<-marcaVehiculo[i+1]
										modeloVehiculo[i]<-modeloVehiculo[i+1]
										patente[i]<-patente[i+1]
										dniDueno[i]<-dniDueno[i+1]
									FinPara
								SiNo
									Escribir "No se encontro la patente en el registro"
								FinSi
							FinPara
							
						De Otro Modo:
							Escribir "Opcion no valida."
							Escribir Sin Saltar "Presione ENTER para volver al menu principal..."
							Leer tecla
					Fin Segun
				SiNo
					Escribir "Cantidad maxima de vehiculos alcanzados"
				FinSi		
				
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
