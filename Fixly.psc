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
	Definir i, j Como Entero
	
	// =====================================================================
	// FIXLY - Variable de opcion vehiculo//
	Definir posicionEliminar, menuVehiculo, contadorVehiculo, dniDueno Como Entero
	Definir marcaVehiculo, modeloVehiculo, patente Como Caracter
	Dimensionar marcaVehiculo[50], modeloVehiculo[50], patente[50], dniDueno[50]
	Definir vehiculoEliminado, lugarVacio, datoBusquedaMod, datoVehiculoMod Como Entero
	Definir patenteEliminar Como Caracter
	contadorVehiculo<-0
	posicionEliminar<-0
	// =====================================================================
	// FIXLY - Variable de opcion cliente
	Definir contadorClientes, menuClientes, dniEliminar, clienteEliminado Como Entero
	Definir nombre, apellido, celular Como Caracter
	Definir dni Como Entero
	Dimensionar nombre[50], apellido[50], celular[50], dni[50]
	contadorClientes<-0
	// =====================================================================
	// FIXLY - Variable de opcion repuestos
	Definir cantidad_repuestos, opcion_repuestos, posicion_encontrada, stock Como Entero
	Definir codigo, nombreRepuesto, codigo_buscar, nombre_buscar Como Caracter
	Definir precio, precio_nuevo Como Real
	Dimension codigo[100]
	Dimension  nombreRepuesto[100]
	Dimension precio[100]
	Dimension stock[100]
	cantidad_repuestos <- 0
	// =====================================================================
	// FIXLY - Variable de opcion ordenes
	Definir opcion_ordenes, dni_buscar, posicion_cliente, posicion_vehiculo, posicion_repuesto, posicion_orden, orden_numero, orden_buscar Como Entero
	Definir cantidad_ordenes, cantidad_detalles, nro_orden_siguiente, cantidad_repuestos_ot, k, dni_orden, opcion_modificar_ot Como Entero
	Definir posicion_repuesto_modificar, detalle_cantidad, detalle_nro_orden, posicion_detalle Como Entero
	Definir detalle_precio_unitario Como Real
	Definir patente_buscar, confirma_repuesto, falla_detalle, nombre_repuesto_buscar, fecha_inicio, fecha_fin, fecha_entrega, patente_orden, estado_orden Como Caracter
	Definir descripcion_orden, diagnostico_orden, nombre_repuesto_modificar, detalle_cod_repuesto, codigo_quitar Como Caracter
	Definir encontrado_detalle Como Logico
	cantidad_ordenes <- 0
	cantidad_repuestos <- 0
	cantidad_detalles <- 0
	nro_orden_siguiente <- 1
	// =====================================================================
	// Vectores órdenes técnicas
	Dimension fecha_inicio[100]
	Dimension fecha_fin[100]
	Dimension fecha_entrega[100]
	Dimension orden_numero[100]
	Dimension dni_orden[100]
	Dimension patente_orden[100]
	Dimension descripcion_orden[100]
	Dimension diagnostico_orden[100]
	Dimension trabajo_orden[100]
	Dimension detalle_nro_orden[300]
	Dimension detalle_cod_repuesto[300]
	Dimension detalle_cantidad[300]
	Dimension detalle_precio_unitario[300]
	Dimension estado_orden[100]
	// =====================================================================
	// FIXLY - Variable de opcion facturacion
	Definir opcion_facturacion, cantidad_facturas, nro_factura_siguiente, factura_buscar, orden_facturar Como Entero
	Definir posicion_factura, posicion_orden_factura, posicion_cliente_factura, dni_factura_buscar, facturas_encontradas Como Entero
	Definir factura_numero, factura_nro_orden, factura_dni Como Entero
	Definir factura_patente, factura_fecha, fecha_factura_ingresada, confirma_factura Como Caracter
	Definir factura_repuestos, factura_horas, factura_valor_hora, factura_mano_obra, factura_subtotal, factura_iva, factura_total Como Real
	Definir total_repuestos_calc, horas_ingresadas, valor_hora_ingresado, mano_obra_calc, subtotal_calc, iva_calc, total_calc Como Real
	Definir porcentaje_iva, total_facturado Como Real
	Definir orden_ya_facturada Como Logico
	Dimension factura_numero[100]
	Dimension factura_nro_orden[100]
	Dimension factura_dni[100]
	Dimension factura_patente[100]
	Dimension factura_fecha[100]
	Dimension factura_repuestos[100]
	Dimension factura_horas[100]
	Dimension factura_valor_hora[100]
	Dimension factura_mano_obra[100]
	Dimension factura_subtotal[100]
	Dimension factura_iva[100]
	Dimension factura_total[100]
	cantidad_facturas <- 0
	nro_factura_siguiente <- 1
	porcentaje_iva <- 21
	
	
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
				Si contadorClientes<50 Entonces
					
					Escribir "----------------------------------------"
					Escribir "        Gestion de Clientes"
					Escribir "----------------------------------------"
					Escribir "  1 - Ingresar un nuevo cliente"
					Escribir "  2 - Ver lista de clientes"
					Escribir "  3 - Quitar un cliente"
					Escribir "  4 - Modificar datos de un cliente"
					Escribir "  0 - Volver"
					Escribir "----------------------------------------"
					Escribir Sin Saltar "Opcion: "
					Leer menuClientes
					Segun menuClientes Hacer
						1:
							Escribir "Ingrese los siguientes datos del cliente"
							Escribir "Nombre del cliente"
							Leer nombre[contadorClientes]
							Escribir "Apellido del cliente"
							Leer apellido[contadorClientes]
							Escribir "Nro de contacto del cliente"
							Leer celular[contadorClientes]
							Repetir
								Escribir "DNI del cliente (Sin puntos)"
								Leer dni[contadorClientes]
								Si (dni[contadorClientes]<=0) o (dni[contadorClientes]>=100000000) Entonces
									Escribir "Valor de DNI invalido"
								FinSi
							Mientras Que dni[contadorClientes]<=0 o dni[contadorClientes]>=100000000
							contadorClientes<-contadorClientes+1
							
						2:
							Escribir "NOMBRE",  "|		|", "APELLIDO", "|		|", "CELULAR", "|		|", "DNI"
							Escribir "---------------------------------------------"
							Para i<-0 Hasta contadorClientes-1 Con Paso 1 Hacer
								Escribir nombre[i], "|		|", apellido[i], "|		|", celular[i], "|		|", dni[i]
							FinPara
						3:
							Escribir Sin Saltar "Escriba el dni del cliente que desea eliminar"
							Leer dniEliminar
							clienteEliminado<-0
							Para i<-0 Hasta contadorClientes-1 Con Paso 1 Hacer
								Si dni[i] = dniEliminar Entonces
									lugarVacio<-i
									Para j<-lugarVacio Hasta contadorClientes-2 Con Paso 1 Hacer
										nombre[j]<-nombre[j+1]
										apellido[j]<-apellido[j+1]
										celular[j]<-celular[j+1]
										dni[j]<-dni[j+1]
									FinPara
									contadorClientes<-contadorClientes-1
									clienteEliminado<-1
								FinSi
							FinPara
							Si clienteEliminado=1 Entonces
								Escribir "Cliente eliminado correctamente"
							SiNo
								Escribir "Error: el DNI no se encuentra en el registro"
							FinSi
						4:
							Repetir
								Escribir Sin Saltar "Seleccione el cliente que desea modificar"
								Escribir "---------------------------------------------"
								Escribir "Nro", "|		|","NOMBRE",  "|		|", "APELLIDO", "|		|", "CELULAR", "|		|", "DNI"
								Escribir "---------------------------------------------"
								Para i<-0 Hasta contadorClientes-1 Con Paso 1 Hacer
									Escribir i+1,"|		|", nombre[i], "|		|", apellido[i], "|		|", celular[i], "|		|", dni[i]
								FinPara
								Leer datoBusquedaMod
							Hasta Que datoBusquedaMod>0 y datoBusquedaMod<contadorClientes
							
							Escribir "---------------------------------------------"
							Escribir "Cliente seleccionado"
							Escribir "---------------------------------------------"
							Escribir "NOMBRE",  "|		|", "APELLIDO", "|		|", "CELULAR", "|		|", "DNI"
							Escribir "---------------------------------------------"
							Escribir nombre[datoBusquedaMod-1], "|		|", apellido[datoBusquedaMod-1], "|		|", celular[datoBusquedaMod-1], "|		|", dni[datoBusquedaMod-1]
							Escribir "---------------------------------------------"
							
							Repetir
								Escribir "Seleccione que dato desea modificar"
								Leer datoVehiculoMod
								Escribir "1) Nombre"
								Escribir "2) Apellido"
								Escribir "3) Celular"
								Escribir "4) DNI"
							Hasta Que datoVehiculoMod>0 y datoVehiculoMod<5
							
							Segun datoVehiculoMod Hacer
								1:
									Escribir "Ingrese el dato corregido"
									Escribir "NOMBRE"
									Leer nombre[datoBusquedaMod-1]
								2:
									Escribir "Ingrese el dato corregido"
									Escribir "APELLIDO"
									Leer apellido[datoBusquedaMod-1]
								3:
									Escribir "Ingrese el dato corregido"
									Escribir "CELULAR"
									Leer celular[datoBusquedaMod-1]
									
								4: 	Escribir "Ingrese el dato corregido"
									Escribir "DNI"
									Leer dni[datoBusquedaMod-1]
									
								De Otro Modo:
									Escribir "Error: opcion invalida"
							Fin Segun
							
						De Otro Modo:
							Escribir "Opcion no valida."
							Escribir Sin Saltar "Presione ENTER para volver al menu principal..."
							Leer tecla
					Fin Segun
				SiNo
					Escribir "Cantidad maxima de clientes alcanzada"
				FinSi
				
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
					
					Escribir "----------------------------------------"
					Escribir "        Gestion de Vehiculos"
					Escribir "----------------------------------------"
					Escribir "  1 - Ingresar un nuevo vehiculo"
					Escribir "  2 - Ver lista de vehiculos"
					Escribir "  3 - Quitar un vehiculo"
					Escribir "  4 - Modificar un vehiculo"
					Escribir "  0 - Volver"
					Escribir "----------------------------------------"
					Escribir Sin Saltar "Opcion: "
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
								Escribir "DNI dueno del vehiculo (Sin puntos)"
								Leer dniDueno[contadorVehiculo]
								Si (dniDueno[contadorVehiculo]<=0) o (dniDueno[contadorVehiculo]>=100000000) Entonces
									Escribir "Valor de DNI invalido"
								FinSi
							Mientras Que dniDueno[contadorVehiculo]<=0 o dniDueno[contadorVehiculo]>=100000000
							contadorVehiculo<-contadorVehiculo+1
							
						2:
							Escribir "MARCA",  "|		|", "MODELO", "|		|", "PATENTE", "|		|", "DNI DUENO"
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
								Escribir "MARCA", "|		|", "MODELO", "|		|", "PATENTE", "|		|", "DNI DUENO"
								Escribir "---------------------------------------------"
								Para i<-0 Hasta contadorVehiculo-1 Con Paso 1 Hacer
									Escribir i+1, "	", marcaVehiculo[i], "	", modeloVehiculo[i], "	", patente[i], "	", dniDueno[i]
								FinPara
								Leer datoBusquedaMod
							Hasta Que datoBusquedaMod>0 y datoBusquedaMod<contadorVehiculo
							
							Escribir "---------------------------------------------"
							Escribir "Vehiculo seleccionado"
							Escribir "---------------------------------------------"
							Escribir "MARCA","|		|", "MODELO", "|		|", "PATENTE", "|		|", "DNI DUENO"
							Escribir "---------------------------------------------"
							Escribir marcaVehiculo[datoBusquedaMod-1], "|			|", modeloVehiculo[datoBusquedaMod-1], "|			|", patente[datoBusquedaMod-1], "|			|", dniDueno[datoBusquedaMod-1]
							Escribir "---------------------------------------------"
							
							Repetir
								Escribir "Seleccione que dato desea modificar"
								Leer datoVehiculoMod
								Escribir "1) Marca"
								Escribir "2) Modelo"
								Escribir "3) Patente"
								Escribir "4) DNI del dueno"
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
									Escribir "DNI del dueno"
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
				Repetir
					Limpiar Pantalla
					Escribir "=================================="
					Escribir "     Modulo órdenes de trabajo"
					Escribir "=================================="
					Escribir "  1 - Cargar orden"
					Escribir "  2 - Modificar orden"
					Escribir "  3 - Buscar orden"
					Escribir "  4 - Cerrar orden"
					Escribir "  0 - Volver al menu principal"
					Escribir Sin Saltar "Opcion: "
					Leer opcion_ordenes
					Segun opcion_ordenes Hacer
						
						1:
							Escribir "Ingrese los siguientes datos de la orden"
							Escribir "DNI del cliente:"
							Leer dni_buscar
							posicion_cliente <- -1
							Para i <-0 Hasta contadorClientes -1 Con Paso 1 Hacer
								
								Si dni_buscar = dni[i] Entonces
									posicion_cliente <- i
									
								FinSi
							FinPara
							
							Si posicion_cliente = -1 Entonces
								Escribir "El DNI no existe en la base. Ingrese los datos del cliente en el módulo correspondiente"
							SiNo
								Escribir "Buscando datos..."
								Esperar 1 segundos
								Escribir "Cliente encontrado... "
								Esperar 1 segundos
								Escribir "=================================="
								Escribir "Apellido y nombre: ", apellido[posicion_cliente], " ", nombre[posicion_cliente]
								Escribir "DNI: ", dni[posicion_cliente]
								Escribir "Celular: ", celular[posicion_cliente]
								Escribir "=================================="
								
								Escribir Sin Saltar "Presione ENTER para continuar..."
								Leer tecla
								Escribir "=================================="
								Escribir "Buscar patente del vehículo:"
								Escribir "=================================="
								Escribir Sin Saltar "Patente del auto:"
								Leer patente_buscar
								
								posicion_vehiculo <- -1
								Para i <-0 Hasta contadorVehiculo -1 Con Paso 1 Hacer
									
									Si patente_buscar = patente[i] Entonces
										posicion_vehiculo <- i
										
									FinSi
								FinPara
								
								Si posicion_vehiculo = -1 Entonces
									Escribir "La patente no existe en la base"
									Escribir "Ingrese los datos del auto en el módulo correspondiente"
									Escribir "Presione ENTER para continuar..."
									Leer tecla
								SiNo
									Si dniDueno[posicion_vehiculo] <> dni_buscar Entonces
										Escribir "Buscando datos..."
										Esperar 1 segundos
										Escribir "Error, la patente no coincide con el dni buscado"
										Escribir "Vuelva a ingresar los datos"
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									SiNo
										
										Escribir "Buscando datos..."
										Esperar 1 segundos
										Escribir "Patente encontrada... "
										Esperar 1 segundos
										Escribir "=================================="
										Escribir "Marca: ", marcaVehiculo[posicion_vehiculo]
										Escribir "Modelo: ", modeloVehiculo[posicion_vehiculo]
										Escribir "Patente: ", patente[posicion_vehiculo]
										Escribir "=================================="
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									FinSi
									Escribir "=================================="
									fecha_inicio[cantidad_ordenes] <- "Hoy"
									Escribir "Fecha de ingreso: automatica (Hoy)"
									
									Escribir Sin Saltar "Fecha estimada de finalizacion (DD/MM/AAAA): "
									Leer fecha_fin[cantidad_ordenes]
									
									Escribir Sin Saltar "Detalle de la falla: "
									Leer falla_detalle
									
									orden_numero[cantidad_ordenes] <- nro_orden_siguiente
									dni_orden[cantidad_ordenes] <- dni_buscar
									patente_orden[cantidad_ordenes] <- patente_buscar
									descripcion_orden[cantidad_ordenes] <- falla_detalle
									diagnostico_orden[cantidad_ordenes] <- ""
									estado_orden[cantidad_ordenes] <- "Pendiente"
									
									Escribir "=================================="
									Escribir "¿Agregar repuestos a la orden?"
									Leer confirma_repuesto
									Mientras confirma_repuesto = "S" o confirma_repuesto = "s" Hacer
										Escribir Sin Saltar "Ingrese el nombre del repuesto: "
										Leer nombre_repuesto_buscar
										posicion_repuesto <- -1
										Para i <-0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
											Si nombre_repuesto_buscar = nombreRepuesto[i]  Entonces
												posicion_repuesto <- i
											FinSi
										FinPara
										Si posicion_repuesto = -1 Entonces
											Escribir  "El nombre del repuesto no existe"
											Escribir "Ingrese los datos del repuesto en el módulo 4"
											Escribir "Presione ENTER para continuar..."
											Leer tecla
										SiNo
											Escribir "Repuesto: ", codigo[posicion_repuesto]
											Escribir "Nombre: ", nombreRepuesto[posicion_repuesto]
											Escribir "Precio: ", precio[posicion_repuesto]
											
											Escribir "Ingresar la cantidad de repuestos utilizados: "
											leer cantidad_repuestos_ot
											
											Si cantidad_repuestos_ot <= 0 Entonces
												Escribir "La cantidad debe ser mayor a cero"
											SiNo
												Si cantidad_repuestos_ot > stock[posicion_repuesto]Entonces
													Escribir "Stock insuficiente, ", stock[posicion_repuesto]
													Escribir "Debe reponer stock"
												SiNo
													detalle_cod_repuesto[cantidad_detalles] <- codigo[posicion_repuesto]
													detalle_cantidad[cantidad_detalles] <- cantidad_repuestos_ot
													detalle_precio_unitario[cantidad_detalles] <- precio[posicion_repuesto]
													detalle_nro_orden[cantidad_detalles] <- nro_orden_siguiente
													stock[posicion_repuesto] <- stock[posicion_repuesto] - cantidad_repuestos_ot
													
													
													cantidad_detalles <- cantidad_detalles + 1
													Escribir "Repuesto agregado correctamente."
												FinSi
											FinSi
										FinSi
										
										Escribir Sin Saltar "¿Agregar otro repuesto? (S/N): "
										Leer confirma_repuesto
									FinMientras
									
									cantidad_ordenes <- cantidad_ordenes + 1
									nro_orden_siguiente <- nro_orden_siguiente + 1
									
									Escribir "=================================="
									Escribir "Orden Nro ", orden_numero[cantidad_ordenes - 1], " creada exitosamente"
									Escribir "=================================="
									Escribir Sin Saltar "Presione ENTER para continuar..."
									Leer tecla
								FinSi
							FinSi
							
							
							
						2:	
							Escribir "Modificar orden"
							Escribir "Ingrese el número de la orden: "
							Leer orden_buscar
							posicion_orden <- -1
							Para i <-0 Hasta cantidad_ordenes -1 Con Paso 1 Hacer
								Si orden_buscar = orden_numero[i] Entonces
									posicion_orden <- i
								FinSi
							FinPara
							
							
							Si estado_orden[posicion_orden] = 'Finalizada' Entonces
								Escribir "Error: solo se pueden modificar órdenes pendientes"
							FinSi
							
							Repetir
								Limpiar Pantalla
								
								Escribir "=================================="
								Escribir "Orden Nro ", orden_numero[posicion_orden]
								Escribir "=================================="
								Escribir "1: Agregar un repuesto"
								Escribir "2: Quitar un repuesto"
								Escribir "3: Modificar detalle de falla"
								Escribir "0: Para volver al menú anterior"
								Escribir Sin Saltar "Opción: "
								Leer opcion_modificar_ot
								
								Segun opcion_modificar_ot Hacer
									1:
										Escribir "Agregar repuesto: "
										Si cantidad_repuestos = 0 Entonces
											Escribir "No hay repuestos cargados en el sistema."
											Escribir "Cargue repuestos en el modulo 4."
											Escribir Sin Saltar "Presione ENTER para continuar..."
											Leer tecla
										SiNo
											Escribir "Repuestos disponibles:"
											Escribir "=================================="
											Para i <- 0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
												Escribir "  ", nombreRepuesto[i], " | Cod: ", codigo[i], " | $", precio[i], " | Stock: ", stock[i]
											FinPara
											Escribir "=================================="
											Escribir "Ingrese el repuesto "
											Leer nombre_repuesto_modificar
											posicion_repuesto_modificar <- -1
											Para i <- 0 Hasta cantidad_repuestos -1 Con Paso 1 Hacer
												Si nombre_repuesto_modificar = nombreRepuesto[i] Entonces
													posicion_repuesto_modificar <- i
												FinSi	
											FinPara
											
											Si posicion_repuesto_modificar = -1 Entonces
												Escribir "El repuesto no existe"
												Escribir "Cargue stock en el módulo repuestos (4)"
												Escribir Sin Saltar "Presione ENTER para continuar..."
												Leer tecla
											SiNo
												Escribir "Repuesto: ", codigo[posicion_repuesto_modificar]
												Escribir "Nombre: ", nombreRepuesto[posicion_repuesto_modificar]
												Escribir "Precio: ", precio[posicion_repuesto_modificar]
												Escribir "Stock: ", stock[posicion_repuesto_modificar]
												Escribir "Ingrese cantidad a agregar: "
												Leer cantidad_repuestos_ot
												
												Si cantidad_repuestos_ot <= 0 Entonces
													Escribir "El valor debe ser mayor a cero(0)"
												SiNo
													Si cantidad_repuestos_ot > stock[posicion_repuesto_modificar] Entonces
														Escribir "Stock insuficiente. Disponible: ", stock[posicion_repuesto_modificar]
														
													SiNo
														detalle_nro_orden[cantidad_detalles] <- orden_numero[posicion_orden]
														detalle_cod_repuesto[cantidad_detalles] <- codigo[posicion_repuesto_modificar]
														detalle_cantidad[cantidad_detalles] <- cantidad_repuestos_ot
														detalle_precio_unitario[cantidad_detalles] <- precio[posicion_repuesto_modificar]
														stock[posicion_repuesto_modificar] <- stock[posicion_repuesto_modificar] - cantidad_repuestos_ot
														cantidad_detalles <- cantidad_detalles + 1
														
														Escribir "Repuesto agregado correctamente."
													FinSi
												FinSi
											FinSi
											Escribir Sin Saltar "Presione ENTER para continuar..."
											Leer tecla
										FinSi
										
										
										
									2:
										Limpiar Pantalla
										Escribir "=================================="
										Escribir "      Quitar repuesto"
										Escribir "=================================="
										
										
										tiene_repuestos <- 0
										Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
											Si detalle_nro_orden[i] = orden_numero[posicion_orden] Entonces
												tiene_repuestos <- tiene_repuestos + 1
											FinSi
										FinPara
										
										Si tiene_repuestos = 0 Entonces
											Escribir "Esta orden no tiene repuestos cargados."
											Escribir Sin Saltar "Presione ENTER para continuar..."
											Leer tecla
										SiNo
											
											Escribir "Repuestos en la orden:"
											Escribir "=================================="
											Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
												Si detalle_nro_orden[i] = orden_numero[posicion_orden] Entonces
													Escribir "  ", detalle_cod_repuesto[i], " x ", detalle_cantidad[i], " = $", detalle_precio_unitario[i] * detalle_cantidad[i]
												FinSi
											FinPara
											Escribir "=================================="
											
											
											Escribir Sin Saltar "Codigo del repuesto a quitar: "
											Leer codigo_quitar
											
											
											posicion_detalle <- -1
											Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
												Si detalle_nro_orden[i] = orden_numero[posicion_orden] Y detalle_cod_repuesto[i] = codigo_quitar Entonces
													posicion_detalle <- i
												FinSi
											FinPara
											
											Si posicion_detalle = -1 Entonces
												Escribir "El repuesto no esta en esta orden."
												Escribir "Verifique el codigo ingresado."
												Escribir Sin Saltar "Presione ENTER para continuar..."
												Leer tecla
											SiNo
												
												Para i <- 0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
													Si codigo[i] = detalle_cod_repuesto[posicion_detalle] Entonces
														stock[i] <- stock[i] + detalle_cantidad[posicion_detalle]
													FinSi
												FinPara
												
												
												Para j <- posicion_detalle Hasta cantidad_detalles - 2 Con Paso 1 Hacer
													detalle_nro_orden[j] <- detalle_nro_orden[j+1]
													detalle_cod_repuesto[j] <- detalle_cod_repuesto[j+1]
													detalle_cantidad[j] <- detalle_cantidad[j+1]
													detalle_precio_unitario[j] <- detalle_precio_unitario[j+1]
												FinPara
												
												
												cantidad_detalles <- cantidad_detalles - 1
												
												Escribir "Repuesto quitado. Stock restaurado."
												Escribir Sin Saltar "Presione ENTER para continuar..."
												Leer tecla
											FinSi
										FinSi
										
										
										
										
									3:
										Limpiar Pantalla
										Escribir "=================================="
										Escribir "  Modificar detalle de falla"
										Escribir "=================================="
										Escribir "Orden Nro: ", orden_numero[posicion_orden]
										Escribir "=================================="
										Escribir "Detalle actual:"
										Escribir "  ", descripcion_orden[posicion_orden]
										Escribir "=================================="
										Escribir Sin Saltar "Nuevo detalle: "
										Leer falla_detalle
										
										
										Si falla_detalle = "" Entonces
											Escribir "Error: El detalle no puede estar vacio."
											Escribir "No se realizaron cambios."
										SiNo
											descripcion_orden[posicion_orden] <- falla_detalle
											Escribir "=================================="
											Escribir "Detalle actualizado correctamente."
											Escribir "Nuevo detalle:"
											Escribir "  ", descripcion_orden[posicion_orden]
											Escribir "=================================="
										FinSi
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
										
										
									0:
										Escribir "Volviendo al menu anterior..."	
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									De Otro Modo:
										Escribir "Valor no válido (ingrese del 0 al 3)"
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
								FinSegun
							Mientras Que opcion_modificar_ot <> 0
							3:
								Limpiar Pantalla
								Escribir "=================================="
								Escribir "     Buscar orden por patente"
								Escribir "=================================="
								Escribir Sin Saltar "Patente del vehiculo: "
								Leer patente_buscar
								
								posicion_orden <- -1
								Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
									Si patente_buscar = patente_orden[i] Entonces
										posicion_orden <- i
									FinSi
								FinPara
								
								Si posicion_orden = -1 Entonces
									Escribir "No hay orden registrada para la patente ", patente_buscar
									Escribir Sin Saltar "Presione ENTER para continuar..."
									Leer tecla
								SiNo
									Limpiar Pantalla
									Escribir "=================================="
									Escribir "ORDEN Nro ", orden_numero[posicion_orden]
									Escribir "=================================="
									Escribir "DNI:            ", dni_orden[posicion_orden]
									Escribir "Patente:        ", patente_orden[posicion_orden]
									Escribir "Fecha ingreso:  ", fecha_inicio[posicion_orden]
									Escribir "Fecha estimada: ", fecha_fin[posicion_orden]
									Escribir "Estado:         ", estado_orden[posicion_orden]
									Escribir "Falla:          ", descripcion_orden[posicion_orden]
									Escribir "=================================="
									Escribir "Repuestos:"
									Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
										Si detalle_nro_orden[i] = orden_numero[posicion_orden] Entonces
											Escribir "  ", detalle_cod_repuesto[i], " x ", detalle_cantidad[i], " = $", detalle_precio_unitario[i] * detalle_cantidad[i]
										FinSi
									FinPara
									Escribir "=================================="
									Escribir Sin Saltar "Presione ENTER para continuar..."
									Leer tecla
								FinSi
								
							4:
								Limpiar Pantalla
								Escribir "=================================="
								Escribir "        Cerrar orden"
								Escribir "=================================="
								Escribir Sin Saltar "Patente del vehiculo: "
								Leer patente_buscar
								
								posicion_orden <- -1
								Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
									Si patente_buscar = patente_orden[i] Entonces
										posicion_orden <- i
									FinSi
								FinPara
								
								Si posicion_orden = -1 Entonces
									Escribir "No hay orden registrada para la patente ", patente_buscar
									Escribir Sin Saltar "Presione ENTER para continuar..."
									Leer tecla
								SiNo
									
									Limpiar Pantalla
									Escribir "=================================="
									Escribir "   ORDEN Nro ", orden_numero[posicion_orden]
									Escribir "=================================="
									Escribir "DNI:            ", dni_orden[posicion_orden]
									Escribir "Patente:        ", patente_orden[posicion_orden]
									Escribir "Fecha ingreso:  ", fecha_inicio[posicion_orden]
									Escribir "Fecha estimada: ", fecha_fin[posicion_orden]
									Escribir "Estado actual:  ", estado_orden[posicion_orden]
									Escribir "Falla:          ", descripcion_orden[posicion_orden]
									Escribir "----------------------------------"
									Escribir "Repuestos utilizados:"
									Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
										Si detalle_nro_orden[i] = orden_numero[posicion_orden] Entonces
											Escribir "  ", detalle_cod_repuesto[i], " x ", detalle_cantidad[i], " = $", detalle_precio_unitario[i] * detalle_cantidad[i]
										FinSi
									FinPara
									Escribir "=================================="
									Escribir Sin Saltar "¿Confirmar cierre de la orden? (S/N): "
									Leer confirma_repuesto
									
									Si confirma_repuesto = "S" o confirma_repuesto = "s" Entonces
										estado_orden[posicion_orden] <- "Finalizada"
										fecha_entrega[posicion_orden] <- fecha_fin[posicion_orden]
										
										Escribir "=================================="
										Escribir "Oorden cerrada correctamente"
										Escribir "=================================="
										Escribir "Nro de orden:   ", orden_numero[posicion_orden]
										Escribir "Estado nuevo:   ", estado_orden[posicion_orden]
										Escribir "Fecha entrega:  ", fecha_entrega[posicion_orden]
										Escribir "=================================="
										Escribir "La orden esta lista para facturacion."
										Escribir "=================================="
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									SiNo
										Escribir "Cierre cancelado."
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									FinSi
								FinSi
								
							0:
								Escribir "Volviendo al menu anterior..."	
								Escribir Sin Saltar "Presione ENTER para continuar..."
								Leer tecla
								
							De Otro Modo:
								Escribir "Opcion no valida"
								Escribir Sin Saltar "Presione ENTER para continuar..."
								Leer tecla
						FinSegun
					Mientras Que opcion_ordenes <> 0
							
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
								Escribir "     Modulo gestion de repuestos"
								Escribir "=================================="
								Escribir "  1 - Cargar stock repuesto"
								Escribir "  2 - Eliminar repuesto"
								Escribir "  3 - Codigo de repuesto"
								Escribir "  4 - Modificar precio"
								Escribir "  5 - Ver listado de repuestos"
								Escribir "  0 - Volver al menu principal"
								Escribir Sin Saltar "Opcion: "
								Leer opcion_repuestos
								Segun opcion_repuestos Hacer
									1:
										Si cantidad_repuestos < 100 Entonces
											Escribir "Cargar repuesto:"
											Escribir Sin Saltar "Codigo repuesto:"
											Leer codigo[cantidad_repuestos]
											Escribir Sin Saltar "Nombre repuesto:"
											Leer nombreRepuesto[cantidad_repuestos]
											Escribir Sin Saltar "Precio repuesto:"
											Leer precio[cantidad_repuestos]
											Escribir "Stock repuesto:"
											Leer stock[cantidad_repuestos]
											cantidad_repuestos <- cantidad_repuestos + 1
											Escribir "Cantidad de repuestos: ", cantidad_repuestos
											Escribir "Codigo guardado: ", codigo[cantidad_repuestos - 1]
											Escribir "Nombre guardado: ", nombreRepuesto[cantidad_repuestos - 1]
										SiNo
											Escribir "Stock completo"
										FinSi
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									2:
										Escribir "Seleccionar codigo para eliminar"
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
												nombreRepuesto[j] <- nombreRepuesto[j+1]
												precio[j] <- precio[j+1]
												stock[j] <- stock[j+1]
											FinPara
											cantidad_repuestos <- cantidad_repuestos - 1
											Escribir "Repuesto eliminado con exito"
										SiNo
											Escribir "El codigo no existe"
										FinSi
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									3:
										Escribir "Ingrese el nombre del repuesto: "
										Leer nombre_buscar
										posicion_encontrada <- -1
										
										Para i <- 0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
											Si nombre_buscar = nombreRepuesto[i] Entonces
												posicion_encontrada <- i
											FinSi
										FinPara
										
										Si posicion_encontrada <> -1 Entonces
											Escribir " ", codigo[posicion_encontrada]
											Escribir " ", nombreRepuesto[posicion_encontrada]
											Escribir " ", precio[posicion_encontrada]
											Escribir " ", stock[posicion_encontrada]
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
											Si nombre_buscar = nombreRepuesto[i] Entonces
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
												Escribir "  Codigo: ", codigo[i]
												Escribir "  Nombre: ", nombreRepuesto[i]
												Escribir "  Precio: $", precio[i]
												Escribir "  Stock: ", stock[i]
												Escribir "----------------------------------"
											FinPara
										FinSi
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									0:
										Escribir "Volviendo al menu principal..."
									De Otro Modo:
										Escribir "Opcion no valida."
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
										
								FinSegun
								
							Mientras Que opcion_repuestos <> 0
							// ------------------------------------------------
							// MODULO REPUESTOS - FIN
							// ------------------------------------------------
							
						5:
							// ------------------------------------------------
							// MODULO FACTURACION - INICIO
							// ------------------------------------------------
							Repetir
								Limpiar Pantalla
								Escribir "=================================="
								Escribir "     Modulo de facturacion"
								Escribir "=================================="
								Escribir "  1 - Emitir factura de una orden"
								Escribir "  2 - Ver una factura"
								Escribir "  3 - Listado de facturas emitidas"
								Escribir "  4 - Facturas de un cliente"
								Escribir "  0 - Volver al menu principal"
								Escribir Sin Saltar "Opcion: "
								Leer opcion_facturacion
								Segun opcion_facturacion Hacer
									1:
										Limpiar Pantalla
										Escribir "=================================="
										Escribir "        Emitir factura"
										Escribir "=================================="
										Si cantidad_facturas >= 100 Entonces
											Escribir "Cantidad maxima de facturas alcanzada"
										SiNo
											Escribir Sin Saltar "Numero de orden a facturar: "
											Leer orden_facturar
							
											posicion_orden_factura <- -1
											Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
												Si orden_numero[i] = orden_facturar Entonces
													posicion_orden_factura <- i
												FinSi
											FinPara
							
											orden_ya_facturada <- Falso
											Para i <- 0 Hasta cantidad_facturas - 1 Con Paso 1 Hacer
												Si factura_nro_orden[i] = orden_facturar Entonces
													orden_ya_facturada <- Verdadero
												FinSi
											FinPara
							
											Si posicion_orden_factura = -1 Entonces
												Escribir "La orden no existe"
											SiNo
												Si estado_orden[posicion_orden_factura] <> "Finalizada" Entonces
													Escribir "La orden todavia esta pendiente."
													Escribir "Cierrela en el modulo de ordenes (3) antes de facturar."
												SiNo
													Si orden_ya_facturada Entonces
														Escribir "La orden ya fue facturada"
													SiNo
														posicion_cliente_factura <- -1
														Para i <- 0 Hasta contadorClientes - 1 Con Paso 1 Hacer
															Si dni[i] = dni_orden[posicion_orden_factura] Entonces
																posicion_cliente_factura <- i
															FinSi
														FinPara
							
														Escribir "Orden Nro:  ", orden_numero[posicion_orden_factura]
														Si posicion_cliente_factura <> -1 Entonces
															Escribir "Cliente:    ", apellido[posicion_cliente_factura], " ", nombre[posicion_cliente_factura]
														FinSi
														Escribir "DNI:        ", dni_orden[posicion_orden_factura]
														Escribir "Patente:    ", patente_orden[posicion_orden_factura]
														Escribir "Falla:      ", descripcion_orden[posicion_orden_factura]
														Escribir "----------------------------------"
														Escribir "Repuestos utilizados:"
														total_repuestos_calc <- 0
														Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
															Si detalle_nro_orden[i] = orden_numero[posicion_orden_factura] Entonces
																Escribir "  ", detalle_cod_repuesto[i], " x ", detalle_cantidad[i], " = $", detalle_precio_unitario[i] * detalle_cantidad[i]
																total_repuestos_calc <- total_repuestos_calc + detalle_precio_unitario[i] * detalle_cantidad[i]
															FinSi
														FinPara
														Si total_repuestos_calc = 0 Entonces
															Escribir "  (sin repuestos)"
														FinSi
														Escribir "----------------------------------"
							
														Repetir
															Escribir Sin Saltar "Horas de mano de obra: "
															Leer horas_ingresadas
															Si horas_ingresadas <= 0 Entonces
																Escribir "Las horas deben ser mayores a cero"
															FinSi
														Hasta Que horas_ingresadas > 0
														Repetir
															Escribir Sin Saltar "Valor de la hora de trabajo: $"
															Leer valor_hora_ingresado
															Si valor_hora_ingresado <= 0 Entonces
																Escribir "El valor debe ser mayor a cero"
															FinSi
														Hasta Que valor_hora_ingresado > 0
														Escribir Sin Saltar "Fecha de emision (DD/MM/AAAA): "
														Leer fecha_factura_ingresada
							
														mano_obra_calc <- horas_ingresadas * valor_hora_ingresado
														subtotal_calc <- total_repuestos_calc + mano_obra_calc
														// IVA redondeado a centavos
														iva_calc <- redon(subtotal_calc * porcentaje_iva) / 100
														total_calc <- subtotal_calc + iva_calc
							
														Escribir "=================================="
														Escribir "Repuestos:        $", total_repuestos_calc
														Escribir "Mano de obra:     $", mano_obra_calc, " (", horas_ingresadas, " hs x $", valor_hora_ingresado, ")"
														Escribir "Subtotal:         $", subtotal_calc
														Escribir "IVA ", porcentaje_iva, "%:          $", iva_calc
														Escribir "TOTAL:            $", total_calc
														Escribir "=================================="
														Escribir Sin Saltar "Confirmar emision de la factura? (S/N): "
														Leer confirma_factura
							
														Si confirma_factura = "S" o confirma_factura = "s" Entonces
															factura_numero[cantidad_facturas] <- nro_factura_siguiente
															factura_nro_orden[cantidad_facturas] <- orden_numero[posicion_orden_factura]
															factura_dni[cantidad_facturas] <- dni_orden[posicion_orden_factura]
															factura_patente[cantidad_facturas] <- patente_orden[posicion_orden_factura]
															factura_fecha[cantidad_facturas] <- fecha_factura_ingresada
															factura_repuestos[cantidad_facturas] <- total_repuestos_calc
															factura_horas[cantidad_facturas] <- horas_ingresadas
															factura_valor_hora[cantidad_facturas] <- valor_hora_ingresado
															factura_mano_obra[cantidad_facturas] <- mano_obra_calc
															factura_subtotal[cantidad_facturas] <- subtotal_calc
															factura_iva[cantidad_facturas] <- iva_calc
															factura_total[cantidad_facturas] <- total_calc
															cantidad_facturas <- cantidad_facturas + 1
															nro_factura_siguiente <- nro_factura_siguiente + 1
															Escribir "Factura Nro ", factura_numero[cantidad_facturas - 1], " emitida correctamente"
														SiNo
															Escribir "Emision cancelada."
														FinSi
													FinSi
												FinSi
											FinSi
										FinSi
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									2:
										Escribir "En construccion"
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									3:
										Escribir "En construccion"
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									4:
										Escribir "En construccion"
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
									0:
										Escribir "Volviendo al menu principal..."
									De Otro Modo:
										Escribir "Opcion no valida."
										Escribir Sin Saltar "Presione ENTER para continuar..."
										Leer tecla
								FinSegun
							Mientras Que opcion_facturacion <> 0
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
