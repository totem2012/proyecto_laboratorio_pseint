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
	Definir posicionEliminar, menuVehiculo, contadorVehiculo, dniDueno, i Como Entero
	Definir marcaVehiculo, modeloVehiculo, patente Como Caracter
	Dimensionar marcaVehiculo[50], modeloVehiculo[50], patente[50], dniDueno[50]
	Definir vehiculoEliminado, lugarVacio, j, datoBusquedaMod, datoVehiculoMod Como Entero
	Definir patenteEliminar Como Caracter
	contadorVehiculo<-0
	posicionEliminar<-0
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
	modeloVehiculo[3] <- "Sandero"
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
				Si contadorVehiculo<50 Entonces
					
					Escribir "Vehiculos: en construccion"
					Escribir "Ingrese una de las siguientes opciones: "
					Escribir "  1 - Ingresar un nuevo vehiculo"
					Escribir "  2 - Ver lista de vehiculos"
					Escribir "  3 - Quitar un vehiculo"
					Escribir "  4 - Modificar un vehiculo (EN CONSTRUCCION)"
					Escribir "  0 - Volver"
					Leer menuVehiculo
					Segun menuVehiculo Hacer
						1:
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
								Si (dniDueno[contadorVehiculo]<=0) o (dniDueno[contadorVehiculo]>=100000000) Entonces
									Escribir "Valor de DNI invalido"
								FinSi
							Mientras Que dniDueno[contadorVehiculo]<=0 o dniDueno[contadorVehiculo]>=100000000
							contadorVehiculo<-contadorVehiculo+1
							
						2:
							Escribir "MARCA",  "|		|", "MODELO", "|		|", "PATENTE", "|		|", "DNI DUEÑO"
							Escribir "---------------------------------------------"
							Para i<-0 Hasta contadorVehiculo-1 Con Paso 1 Hacer
								Escribir marcaVehiculo[i], "|		|", modeloVehiculo[i], "|		|", patente[i], "|		|", dniDueno[i]
							FinPara
						3:
							Escribir Sin Saltar "Escriba la patente del vehiculo que desea eliminar"
							Leer patenteEliminar
							vehiculoEliminado<-0
							Para i<-0 Hasta contadorVehiculo-1 Con Paso 1 Hacer
								Si patente[i] = patenteEliminar Entonces
									lugarVacio<-i
									Para j<-lugarVacio Hasta contadorVehiculo-2 Con Paso 1 Hacer
										marcaVehiculo[j]<-marcaVehiculo[j+1]
										modeloVehiculo[j]<-modeloVehiculo[j+1]
										patente[j]<-patente[j+1]
										dniDueno[j]<-dniDueno[j+1]
									FinPara
									contadorVehiculo<-contadorVehiculo-1
									vehiculoEliminado<-1
								FinSi
							FinPara
							Si vehiculoEliminado=1 Entonces
								Escribir "Vehiculo eliminado correctamente"
							SiNo
								Escribir "Error: la patente no se encuentra en el registro"
							FinSi
						4: 
							Repetir
								Escribir Sin Saltar "Seleccione el vehiculo que desea modificar"
								Escribir "---------------------------------------------"
								Escribir "MARCA", "|		|", "MODELO", "||		|", "PATENTE", "|		", "DNI DUEÑO"
								Escribir "---------------------------------------------"
								Para i<-0 Hasta contadorVehiculo-1 Con Paso 1 Hacer
									Escribir i+1, "	", marcaVehiculo[i], "	", modeloVehiculo[i], "	", patente[i], "	", dniDueno[i]
								FinPara
								Leer datoBusquedaMod
							Hasta Que datoBusquedaMod>0 y datoBusquedaMod<contadorVehiculo
							
							Escribir "---------------------------------------------"
							Escribir "Vehiculo seleccionado"
							Escribir "---------------------------------------------"
							Escribir "MARCA","|		|", "MODELO", "|		|", "PATENTE", "|		|", "DNI DUEÑO"
							Escribir "---------------------------------------------"
							Escribir marcaVehiculo[datoBusquedaMod-1], "|			|", modeloVehiculo[datoBusquedaMod-1], "|			|", patente[datoBusquedaMod-1], "|			|", dniDueno[datoBusquedaMod-1]
							Escribir "---------------------------------------------"
							
							Repetir
								Escribir "Seleccione que dato desea modificar"
								Leer datoVehiculoMod
								Escribir "1) Marca"
								Escribir "2) Modelo"
								Escribir "3) Patente"
								Escribir "4) DNI del dueño"
							Hasta Que datoVehiculoMod>0 y datoVehiculoMod<5
							
							Segun datoVehiculoMod Hacer
								1:
									Escribir "Ingrese el dato corregido"
									Escribir "MARCA"
									Leer marcaVehiculo[datoBusquedaMod-1]
								2:
									Escribir "Ingrese el dato corregido"
									Escribir "MODELO"
									Leer modeloVehiculo[datoBusquedaMod-1]
								3:
									Escribir "Ingrese el dato corregido"
									Escribir "Patente"
									Leer patente[datoBusquedaMod-1]
									
								4: 	Escribir "Ingrese el dato corregido"
									Escribir "DNI del dueño"
									Leer dniDueno[datoBusquedaMod-1]
									
								De Otro Modo:
									Escribir "Error: opcion invalida"
							Fin Segun
							
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
