// =====================================================================
// fixly - sistema de gestion de taller mecanico
// =====================================================================

// ---------------------------------------------------------------------
// funciones generales del sistema
// ---------------------------------------------------------------------
Funcion MostrarLogo
	Escribir "=================================================================="
	Escribir "  ______ _____  __  __  _      __     __"
	Escribir " |  ____|_   _| \ \/ / | |     \ \   / /"
	Escribir " | |__    | |    \  /  | |      \ \_/ / "
	Escribir " |  __|   | |     ><   | |       \   /  "
	Escribir " | |     _| |_   /  \  | |____    | |   "
	Escribir " |_|    |_____| /_/\_\ |______|   |_|    GESTION DE TALLER"
	Escribir "=================================================================="
FinFuncion

Funcion Pausa
	Definir teclaPausa Como Caracter
	Escribir ""
	Escribir Sin Saltar ">> Presione ENTER para continuar..."
	Leer teclaPausa
FinFuncion

Funcion dniLeido <- LeerDni(mensaje)
	Definir dniLeido Como Entero
	Repetir
		Escribir mensaje
		Leer dniLeido
		Si dniLeido <= 0 o dniLeido >= 100000000 Entonces
			Escribir "Valor de DNI inválido"
		FinSi
	Mientras Que dniLeido <= 0 o dniLeido >= 100000000
FinFuncion

Funcion posicion <- BuscarPosicion(lista, cantidad, buscado)
	Definir posicion, indice Como Entero
	posicion <- -1
	Para indice <- 0 Hasta cantidad - 1 Con Paso 1 Hacer
		Si lista[indice] = buscado Entonces
			posicion <- indice
		FinSi
	FinPara
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo clientes
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuClientes
	Limpiar Pantalla
	Definir opcion Como Entero
	Escribir "=================================================================="
	Escribir "                       GESTIÓN DE CLIENTES                        "
	Escribir "=================================================================="
	Escribir "  [1] Ingresar un nuevo cliente"
	Escribir "  [2] Ver lista de clientes"
	Escribir "  [3] Quitar un cliente"
	Escribir "  [4] Modificar datos de un cliente"
	Escribir "  ----------------------------------------------------------------"
	Escribir "  [0] Volver al menu principal"
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese opción deseada: "
	Leer opcion
FinFuncion

Funcion IngresarCliente(nombre, apellido, celular, dni, contadorClientes Por Referencia)
	Definir dniNuevo, posDniExistente Como Entero
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                     REGISTRAR NUEVO CLIENTE                      "
	Escribir "=================================================================="
	Si contadorClientes >= 1000 Entonces
		Escribir "  [!] Capacidad máxima de clientes alcanzada (limite 1000)."
		Pausa
	SiNo
		Escribir ">> Ingrese los datos del nuevo cliente:"
		Escribir ""
		Escribir Sin Saltar "Nombre del cliente: "
		Leer nombre[contadorClientes]
		Escribir Sin Saltar "Apellido del cliente: "
		Leer apellido[contadorClientes]
		Escribir Sin Saltar "Nro de contacto / Celular: "
		Leer celular[contadorClientes]
		Repetir
			dniNuevo <- LeerDni("DNI del cliente (Sin puntos): ")
			posDniExistente <- BuscarPosicion(dni, contadorClientes, dniNuevo)
			Si posDniExistente <> -1 Entonces
				Escribir "  [!] Ya existe un cliente registrado con el DNI ", dniNuevo, " (", apellido[posDniExistente], " ", nombre[posDniExistente], ")."
			FinSi
		Hasta Que posDniExistente = -1
		dni[contadorClientes] <- dniNuevo
		contadorClientes <- contadorClientes + 1
		Escribir "------------------------------------------------------------------"
		Escribir "[OK] Cliente registrado correctamente en el sistema."
		Escribir "=================================================================="
		Pausa
	FinSi
FinFuncion

Funcion MostrarDatosClientes(nombre, apellido, celular, dni, contadorClientes)
	Definir i Como Entero
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                       LISTADO DE CLIENTES                        "
	Escribir "=================================================================="
	Si contadorClientes = 0 Entonces
		Escribir "  [!] No hay clientes registrados actualmente."
		Escribir "=================================================================="
	SiNo
		Escribir "Nro | DNI       | NOMBRE Y APELLIDO       | CONTACTO / CELULAR"
		Escribir "----+-----------+-------------------------+-----------------------"
		Para i <- 0 Hasta contadorClientes - 1 Con Paso 1 Hacer
			Escribir i + 1, "   | ", dni[i], "  | ", nombre[i], " ", apellido[i], " | ", celular[i]
		FinPara
		Escribir "=================================================================="
		Escribir "Total de clientes registrados: ", contadorClientes
		Escribir "=================================================================="
	FinSi
FinFuncion

Funcion EliminarCliente(nombre, apellido, celular, dni, contadorClientes Por Referencia)
	Definir dniEliminar, i, j, lugarVacio Como Entero
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                         QUITAR CLIENTE                           "
	Escribir "=================================================================="
	dniEliminar <- LeerDni("Escriba el DNI del cliente que desea eliminar: ")
	lugarVacio <- -1
	Para i <- 0 Hasta contadorClientes - 1 Con Paso 1 Hacer
		Si dni[i] = dniEliminar Entonces
			lugarVacio <- i
		FinSi
	FinPara
	Si lugarVacio <> -1 Entonces
		Para j <- lugarVacio Hasta contadorClientes - 2 Con Paso 1 Hacer
			nombre[j] <- nombre[j+1]
			apellido[j] <- apellido[j+1]
			celular[j] <- celular[j+1]
			dni[j] <- dni[j+1]
		FinPara
		contadorClientes <- contadorClientes - 1
		Escribir "------------------------------------------------------------------"
		Escribir "[OK] Cliente eliminado correctamente del sistema."
		Escribir "=================================================================="
	SiNo
		Escribir "------------------------------------------------------------------"
		Escribir "[!] Error: El DNI no se encuentra en el registro."
		Escribir "=================================================================="
	FinSi
	Pausa
FinFuncion

Funcion MostrarFichaCliente(nombre, apellido, celular, dni, pos)
	Escribir "+----------------------------------------------------------------+"
	Escribir "|                   FICHA DEL CLIENTE SELECCIONADO               |"
	Escribir "+----------------------------------------------------------------+"
	Escribir "| NOMBRE:   ", nombre[pos]
	Escribir "| APELLIDO: ", apellido[pos]
	Escribir "| CELULAR:  ", celular[pos]
	Escribir "| DNI:      ", dni[pos]
	Escribir "+----------------------------------------------------------------+"
FinFuncion

Funcion opcion <- PedirCampoCliente
	Definir opcion Como Entero
	Repetir
		Escribir "Seleccione el dato que desea modificar:"
		Escribir "  [1] Nombre"
		Escribir "  [2] Apellido"
		Escribir "  [3] Celular"
		Escribir "  [4] DNI"
		Escribir Sin Saltar ">> Opción: "
		Leer opcion
	Hasta Que opcion > 0 Y opcion < 5
FinFuncion

Funcion ModificarCliente(nombre, apellido, celular, dni, contadorClientes)
	Definir datoBusquedaMod, datoCampoMod, dniNuevo, posDniExistente Como Entero
	Si contadorClientes = 0 Entonces
		Limpiar Pantalla
		Escribir "=================================================================="
		Escribir "                     MODIFICAR CLIENTE                            "
		Escribir "=================================================================="
		Escribir "  [!] No hay clientes registrados para modificar."
		Escribir "=================================================================="
		Pausa
	SiNo
		Repetir
			MostrarDatosClientes(nombre, apellido, celular, dni, contadorClientes)
			Escribir "Seleccione el cliente que desea modificar:"
			Escribir Sin Saltar ">> Nro de cliente: "
			Leer datoBusquedaMod
		Hasta Que datoBusquedaMod > 0 Y datoBusquedaMod <= contadorClientes
		
		MostrarFichaCliente(nombre, apellido, celular, dni, datoBusquedaMod - 1)
		datoCampoMod <- PedirCampoCliente
		
		Segun datoCampoMod Hacer
			1:
				Escribir Sin Saltar "Ingrese el nuevo NOMBRE: "
				Leer nombre[datoBusquedaMod - 1]
			2:
				Escribir Sin Saltar "Ingrese el nuevo APELLIDO: "
				Leer apellido[datoBusquedaMod - 1]
			3:
				Escribir Sin Saltar "Ingrese el nuevo CELULAR: "
				Leer celular[datoBusquedaMod - 1]
			4:
				Repetir
					dniNuevo <- LeerDni("Ingrese el nuevo DNI: ")
					posDniExistente <- BuscarPosicion(dni, contadorClientes, dniNuevo)
					Si posDniExistente <> -1 Y posDniExistente <> datoBusquedaMod - 1 Entonces
						Escribir "  [!] Ya existe un cliente registrado con el DNI ", dniNuevo, " (", apellido[posDniExistente], " ", nombre[posDniExistente], ")."
					FinSi
				Hasta Que posDniExistente = -1 o posDniExistente = datoBusquedaMod - 1
				dni[datoBusquedaMod - 1] <- dniNuevo
		FinSegun
		Escribir "------------------------------------------------------------------"
		Escribir "[OK] Dato del cliente actualizado correctamente."
		Escribir "=================================================================="
		Pausa
	FinSi
FinFuncion

Funcion menuClientes(nombre, apellido, celular, dni, contadorClientes Por Referencia)
	Definir opcion_clientes Como Entero
	Repetir
		opcion_clientes <- MostrarMenuClientes
		Segun opcion_clientes Hacer
			1:
				IngresarCliente(nombre, apellido, celular, dni, contadorClientes)
			2:
				MostrarDatosClientes(nombre, apellido, celular, dni, contadorClientes)
				Pausa
			3:
				EliminarCliente(nombre, apellido, celular, dni, contadorClientes)
			4:
				ModificarCliente(nombre, apellido, celular, dni, contadorClientes)
			0:
				Escribir "Volviendo al menu principal..."
			De Otro Modo:
				Escribir "Opción no valida."
				Pausa
		FinSegun
	Hasta Que opcion_clientes = 0
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo vehiculos
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuVehiculos
	Limpiar Pantalla
	Definir opcion Como Entero
	Escribir "=================================================================="
	Escribir "                      GESTIÓN DE VEHICULOS                        "
	Escribir "=================================================================="
	Escribir "  [1] Ingresar un nuevo vehículo"
	Escribir "  [2] Ver lista de vehículos"
	Escribir "  [3] Quitar un vehículo"
	Escribir "  [4] Modificar datos de un vehículo"
	Escribir "  ----------------------------------------------------------------"
	Escribir "  [0] Volver al menu principal"
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese opción deseada: "
	Leer opcion
FinFuncion

Funcion IngresarVehiculo(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo Por Referencia, dni, nombre, apellido, celular, contadorClientes Por Referencia)
	Definir patenteNueva, respCliente Como Caracter
	Definir dniIngresado, posCliente, posPatenteExistente Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                    REGISTRAR NUEVO VEHÍCULO                      "
	Escribir "=================================================================="
	Si contadorVehiculo >= 1000 Entonces
		Escribir "  [!] Capacidad máxima de vehículos alcanzada (limite 1000)."
		Pausa
	SiNo
		Escribir ">> Ingrese los datos del vehículo a registrar:"
		Escribir ""
		Escribir Sin Saltar "Patente del vehículo: "
		Leer patenteNueva
		
		posPatenteExistente <- BuscarPosicion(patente, contadorVehiculo, patenteNueva)
		Si posPatenteExistente <> -1 Entonces
			Escribir "------------------------------------------------------------------"
			Escribir "  [!] Error: Ya existe un vehículo registrado con la patente ", patenteNueva
			Escribir "=================================================================="
			Pausa
		SiNo
			patente[contadorVehiculo] <- patenteNueva
			Escribir Sin Saltar "Marca del vehículo: "
			Leer marcaVehiculo[contadorVehiculo]
			Escribir Sin Saltar "Modelo del vehículo: "
			Leer modeloVehiculo[contadorVehiculo]
			
			dniIngresado <- LeerDni("DNI del titular / dueño (Sin puntos): ")
			posCliente <- BuscarPosicion(dni, contadorClientes, dniIngresado)
			
			Si posCliente = -1 Entonces
				Escribir "------------------------------------------------------------------"
				Escribir "  [!] Aviso: El DNI ", dniIngresado, " no corresponde a un cliente registrado."
				Escribir Sin Saltar "  Desea dar de alta al cliente en este momento? (S/N): "
				Leer respCliente
				Si respCliente = "S" o respCliente = "s" Entonces
					Si contadorClientes < 1000 Entonces
						Escribir Sin Saltar "Nombre del titular: "
						Leer nombre[contadorClientes]
						Escribir Sin Saltar "Apellido del titular: "
						Leer apellido[contadorClientes]
						Escribir Sin Saltar "Celular de contacto: "
						Leer celular[contadorClientes]
						dni[contadorClientes] <- dniIngresado
						contadorClientes <- contadorClientes + 1
						Escribir "  [OK] Titular registrado y asociado exitosamente."
					FinSi
				SiNo
					Escribir "  Aviso: El vehículo queda registrado con DNI asignado (sin ficha personal)."
				FinSi
			SiNo
				Escribir "  >> Titular identificado: ", apellido[posCliente], " ", nombre[posCliente]
			FinSi
			
			dniDueno[contadorVehiculo] <- dniIngresado
			contadorVehiculo <- contadorVehiculo + 1
			Escribir "------------------------------------------------------------------"
			Escribir "[OK] Vehículo registrado correctamente en el sistema."
			Escribir "=================================================================="
			Pausa
		FinSi
	FinSi
FinFuncion

Funcion MostrarDatosVehiculos(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, contadorClientes)
	Definir i, posCliente Como Entero
	Limpiar Pantalla
	Escribir "=================================================================================="
	Escribir "                              LISTADO DE VEHÍCULOS                                "
	Escribir "=================================================================================="
	Si contadorVehiculo = 0 Entonces
		Escribir "  [!] No hay vehículos registrados en el sistema."
		Escribir "=================================================================================="
	SiNo
		Escribir "Nro | PATENTE  | MARCA        | MODELO       | DNI DUEÑO | TITULAR / DUEÑO"
		Escribir "----+----------+--------------+--------------+-----------+------------------------"
		Para i <- 0 Hasta contadorVehiculo - 1 Con Paso 1 Hacer
			posCliente <- BuscarPosicion(dni, contadorClientes, dniDueno[i])
			Si posCliente <> -1 Entonces
				Escribir i + 1, "   | ", patente[i], "  | ", marcaVehiculo[i], " | ", modeloVehiculo[i], " | ", dniDueno[i], "  | ", apellido[posCliente], " ", nombre[posCliente]
			SiNo
				Escribir i + 1, "   | ", patente[i], "  | ", marcaVehiculo[i], " | ", modeloVehiculo[i], " | ", dniDueno[i], "  | (Sin ficha de cliente)"
			FinSi
		FinPara
		Escribir "=================================================================================="
		Escribir "Total de vehículos registrados: ", contadorVehiculo
		Escribir "=================================================================================="
	FinSi
FinFuncion

Funcion EliminarVehiculo(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo Por Referencia)
	Definir patenteEliminar Como Caracter
	Definir i, j, posVehiculo Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                        QUITAR VEHÍCULO                           "
	Escribir "=================================================================="
	Escribir Sin Saltar "Escriba la patente del vehículo que desea eliminar: "
	Leer patenteEliminar
	posVehiculo <- BuscarPosicion(patente, contadorVehiculo, patenteEliminar)
	
	Si posVehiculo <> -1 Entonces
		Para j <- posVehiculo Hasta contadorVehiculo - 2 Con Paso 1 Hacer
			marcaVehiculo[j] <- marcaVehiculo[j+1]
			modeloVehiculo[j] <- modeloVehiculo[j+1]
			patente[j] <- patente[j+1]
			dniDueno[j] <- dniDueno[j+1]
		FinPara
		contadorVehiculo <- contadorVehiculo - 1
		Escribir "------------------------------------------------------------------"
		Escribir "[OK] Vehículo eliminado correctamente del registro."
		Escribir "=================================================================="
	SiNo
		Escribir "------------------------------------------------------------------"
		Escribir "[!] Error: La patente no se encuentra registrada."
		Escribir "=================================================================="
	FinSi
	Pausa
FinFuncion

Funcion ModificarVehiculo(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, contadorClientes)
	Definir datoBusquedaMod, datoVehiculoMod, posCliente, nuevoDni Como Entero
	Si contadorVehiculo = 0 Entonces
		Limpiar Pantalla
		Escribir "=================================================================="
		Escribir "                     MODIFICAR VEHÍCULO                           "
		Escribir "=================================================================="
		Escribir "  [!] No hay vehículos registrados para modificar."
		Escribir "=================================================================="
		Pausa
	SiNo
		Repetir
			MostrarDatosVehiculos(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, contadorClientes)
			Escribir "Seleccione el vehículo que desea modificar:"
			Escribir Sin Saltar ">> Nro de vehículo: "
			Leer datoBusquedaMod
		Hasta Que datoBusquedaMod > 0 Y datoBusquedaMod <= contadorVehiculo
		
		Escribir "+----------------------------------------------------------------+"
		Escribir "| Vehiculo: ", marcaVehiculo[datoBusquedaMod-1], " ", modeloVehiculo[datoBusquedaMod-1], " | Patente: ", patente[datoBusquedaMod-1]
		Escribir "+----------------------------------------------------------------+"
		
		Repetir
			Escribir "Seleccione el campo que desea modificar:"
			Escribir "  [1] Marca"
			Escribir "  [2] Modelo"
			Escribir "  [3] Patente"
			Escribir "  [4] DNI del titular"
			Escribir Sin Saltar ">> Opción: "
			Leer datoVehiculoMod
		Hasta Que datoVehiculoMod > 0 Y datoVehiculoMod < 5
		
		Segun datoVehiculoMod Hacer
			1:
				Escribir Sin Saltar "Ingrese la nueva MARCA: "
				Leer marcaVehiculo[datoBusquedaMod-1]
			2:
				Escribir Sin Saltar "Ingrese el nuevo MODELO: "
				Leer modeloVehiculo[datoBusquedaMod-1]
			3:
				Escribir Sin Saltar "Ingrese la nueva PATENTE: "
				Leer patente[datoBusquedaMod-1]
			4:
				nuevoDni <- LeerDni("Ingrese el nuevo DNI del titular: ")
				dniDueno[datoBusquedaMod-1] <- nuevoDni
		FinSegun
		Escribir "------------------------------------------------------------------"
		Escribir "[OK] Datos del vehículo actualizados correctamente."
		Escribir "=================================================================="
		Pausa
	FinSi
FinFuncion

Funcion menuVehiculos(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo Por Referencia, dni, nombre, apellido, celular, contadorClientes Por Referencia)
	Definir opcion_vehiculos Como Entero
	Repetir
		opcion_vehiculos <- MostrarMenuVehiculos
		Segun opcion_vehiculos Hacer
			1:
				IngresarVehiculo(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, celular, contadorClientes)
			2:
				MostrarDatosVehiculos(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, contadorClientes)
				Pausa
			3:
				EliminarVehiculo(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo)
			4:
				ModificarVehiculo(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, contadorClientes)
			0:
				Escribir "Volviendo al menu principal..."
			De Otro Modo:
				Escribir "Opción no válida."
				Pausa
		FinSegun
	Hasta Que opcion_vehiculos = 0
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo ordenes de trabajo
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuOrdenes
	Definir opcion Como Entero
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                   ÓRDENES DE TRABAJO (TALLER)                    "
	Escribir "=================================================================="
	Escribir "  [1] Cargar nueva orden de trabajo"
	Escribir "  [2] Modificar orden de trabajo"
	Escribir "  [3] Buscar orden por patente"
	Escribir "  [4] Cerrar / Finalizar orden de trabajo"
	Escribir "  ----------------------------------------------------------------"
	Escribir "  [0] Volver al menu principal"
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese opción deseada: "
	Leer opcion
FinFuncion

Funcion CargarOrden(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes Por Referencia, nro_orden_siguiente Por Referencia, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles Por Referencia, dni, nombre, apellido, contadorClientes, patente, marcaVehiculo, modeloVehiculo, dniDueno, contadorVehiculo, codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
	Definir posicion_cliente, posicion_vehiculo, posicion_repuesto, dni_buscar Como Entero
	Definir cantidad_repuestos_ot Como Entero
	Definir patente_buscar, confirma_repuesto, falla_detalle, diag_detalle, nombre_repuesto_buscar, f_inicio, f_estimada Como Caracter
	Definir vehiculosEnTaller, i Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                 CARGAR NUEVA ORDEN DE TRABAJO                    "
	Escribir "=================================================================="
	
	vehiculosEnTaller <- 0
	
	Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
		Si estado_orden[i] = "Pendiente" Entonces
			vehiculosEnTaller <- vehiculosEnTaller + 1
		FinSi
	FinPara
	
	Si vehiculosEnTaller >= 20 Entonces
		Escribir "------------------------------------------------------------------"
		Escribir "  [!] No hay espacio disponible en el taller."
		Escribir "  Vehículos actualmente en reparación: ", vehiculosEnTaller
		Escribir "  Capacidad máxima permitida: 20"
		Escribir "=================================================================="
		Pausa
	SiNo
		dni_buscar <- LeerDni(">> DNI del cliente (Sin puntos): ")
		posicion_cliente <- BuscarPosicion(dni, contadorClientes, dni_buscar)
		
		Si posicion_cliente = -1 Entonces
			Escribir "------------------------------------------------------------------"
			Escribir "  [!] El DNI ", dni_buscar, " no existe en la base."
			Escribir "  Por favor, cargue primero el cliente en el Modulo 1."
			Escribir "=================================================================="
			Pausa
		SiNo
			Escribir ">> Cliente encontrado: ", apellido[posicion_cliente], ", ", nombre[posicion_cliente]
			Escribir Sin Saltar ">> Patente del vehículo: "
			Leer patente_buscar
			
			posicion_vehiculo <- BuscarPosicion(patente, contadorVehiculo, patente_buscar)
			
			Si posicion_vehiculo = -1 Entonces
				Escribir "------------------------------------------------------------------"
				Escribir "  [!] La patente ", patente_buscar, " no existe en la base."
				Escribir "  Por favor, cargue primero el vehículo en el Módulo 2."
				Escribir "=================================================================="
				Pausa
			SiNo
				Si dniDueno[posicion_vehiculo] <> dni_buscar Entonces
					Escribir "------------------------------------------------------------------"
					Escribir "  [!] Error: El vehículo con patente ", patente_buscar, " no pertenece al DNI ", dni_buscar, "."
					Escribir "=================================================================="
					Pausa
				SiNo
					Escribir ">> Vehículo: ", marcaVehiculo[posicion_vehiculo], " ", modeloVehiculo[posicion_vehiculo]
					Escribir "------------------------------------------------------------------"
					
					Escribir Sin Saltar ">> Fecha de ingreso (DD/MM/AAAA) [ENTER para Hoy]: "
					Leer f_inicio
					Si f_inicio = "" Entonces
						f_inicio <- "Hoy"
					FinSi
					
					Escribir Sin Saltar ">> Fecha estimada de finalización (DD/MM/AAAA): "
					Leer f_estimada
					
					Escribir Sin Saltar ">> Motivo de ingreso / Detalle de la falla: "
					Leer falla_detalle
					
					Escribir Sin Saltar ">> Diagnóstico inicial: "
					Leer diag_detalle
					
					dni_orden[cantidad_ordenes] <- dni_buscar
					patente_orden[cantidad_ordenes] <- patente_buscar
					fecha_inicio[cantidad_ordenes] <- f_inicio
					fecha_fin[cantidad_ordenes] <- f_estimada
					fecha_entrega[cantidad_ordenes] <- ""
					descripcion_orden[cantidad_ordenes] <- falla_detalle
					diagnostico_orden[cantidad_ordenes] <- diag_detalle
					trabajo_orden[cantidad_ordenes] <- ""
					
					orden_numero[cantidad_ordenes] <- nro_orden_siguiente
					nro_orden_siguiente <- nro_orden_siguiente + 1
					estado_orden[cantidad_ordenes] <- "Pendiente"
					
					Escribir "------------------------------------------------------------------"
					Escribir Sin Saltar ">> Desea agregar repuestos a la orden ahora? (S/N): "
					Leer confirma_repuesto
					
					Mientras confirma_repuesto = "S" o confirma_repuesto = "s" Hacer
						Escribir Sin Saltar ">> Código o nombre del repuesto (o FIN para terminar): "
						Leer nombre_repuesto_buscar
						
						Si nombre_repuesto_buscar <> "FIN" Y nombre_repuesto_buscar <> "fin" Y nombre_repuesto_buscar <> "" Entonces
							posicion_repuesto <- BuscarPosicion(codigo, cantidad_repuestos, nombre_repuesto_buscar)
							Si posicion_repuesto = -1 Entonces
								posicion_repuesto <- BuscarPosicion(nombreRepuesto, cantidad_repuestos, nombre_repuesto_buscar)
							FinSi
							
							Si posicion_repuesto = -1 Entonces
								Escribir "  [!] El repuesto no existe en el catálogo. Cárguelo en el módulo 4."
							SiNo
								Escribir "  Artículo: ", nombreRepuesto[posicion_repuesto], " | Stock disponible: ", stock[posicion_repuesto], " | Precio: $", precio[posicion_repuesto]
								Escribir Sin Saltar "  Cantidad requerida: "
								Leer cantidad_repuestos_ot
								
								Si cantidad_repuestos_ot <= 0 Entonces
									Escribir "  [!] La cantidad debe ser mayor a cero."
								SiNo
									Si cantidad_repuestos_ot > stock[posicion_repuesto] Entonces
										Escribir "  [!] Stock insuficiente. Disponible: ", stock[posicion_repuesto]
									SiNo
										Si cantidad_detalles >= 300 Entonces
											Escribir "  [!] Límite máximo de repuestos asignados a órdenes alcanzado (300)."
										SiNo
											detalle_nro_orden[cantidad_detalles] <- orden_numero[cantidad_ordenes]
											detalle_cod_repuesto[cantidad_detalles] <- codigo[posicion_repuesto]
											detalle_cantidad[cantidad_detalles] <- cantidad_repuestos_ot
											detalle_precio_unitario[cantidad_detalles] <- precio[posicion_repuesto]
											stock[posicion_repuesto] <- stock[posicion_repuesto] - cantidad_repuestos_ot
											cantidad_detalles <- cantidad_detalles + 1
											Escribir "  [OK] Repuesto asignado a la orden y descontado del stock."
										FinSi
									FinSi
								FinSi
							FinSi
							Escribir Sin Saltar ">> Desea agregar otro repuesto? (S/N): "
							Leer confirma_repuesto
						SiNo
							confirma_repuesto <- "N"
						FinSi
					FinMientras
					
					cantidad_ordenes <- cantidad_ordenes + 1
					Escribir "------------------------------------------------------------------"
					Escribir "[OK] Orden Nro ", orden_numero[cantidad_ordenes - 1], " registrada con éxito."
					Escribir "=================================================================="
					Pausa
				FinSi
			FinSi
		FinSi
	FinSi
FinFuncion

Funcion ModificarOrden(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles Por Referencia, codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
	Definir posicion_orden, posicion_repuesto_modificar, posicion_detalle, opcion_modificar_ot Como Entero
	Definir i, j, cantidad_repuestos_ot, tiene_repuestos Como Entero
	Definir dato_buscar, codigo_quitar, falla_detalle, diag_detalle, nombre_repuesto_modificar Como Caracter
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                   MODIFICAR ORDEN DE TRABAJO                     "
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese patente o número de orden a modificar: "
	Leer dato_buscar
	
	posicion_orden <- BuscarPosicion(patente_orden, cantidad_ordenes, dato_buscar)
	Si posicion_orden = -1 Entonces
		Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
			Si dato_buscar = ConvertirATexto(orden_numero[i]) Entonces
				posicion_orden <- i
			FinSi
		FinPara
	FinSi
	
	Si posicion_orden = -1 Entonces
		Escribir "------------------------------------------------------------------"
		Escribir "  [!] Error: No se encontró orden registrada para: ", dato_buscar
		Escribir "=================================================================="
		Pausa
	SiNo
		Si estado_orden[posicion_orden] = "Finalizada" Entonces
			Escribir "------------------------------------------------------------------"
			Escribir "  [!] La orden Nro ", orden_numero[posicion_orden], " ya está Finalizada y no puede modificarse."
			Escribir "=================================================================="
			Pausa
		SiNo
			Repetir
				Limpiar Pantalla
				Escribir "=================================================================="
				Escribir "                 MODIFICAR ORDEN NRO ", orden_numero[posicion_orden]
				Escribir "=================================================================="
				Escribir "  [1] Modificar motivo de falla"
				Escribir "  [2] Modificar diagnóstico"
				Escribir "  [3] Agregar repuestos a la orden"
				Escribir "  [4] Quitar repuesto de la orden"
				Escribir "  ----------------------------------------------------------------"
				Escribir "  [0] Volver al menu de órdenes"
				Escribir "=================================================================="
				Escribir Sin Saltar ">> Ingrese opción deseada: "
				Leer opcion_modificar_ot
				
				Segun opcion_modificar_ot Hacer
					1:
						Escribir ">> Motivo actual: ", descripcion_orden[posicion_orden]
						Escribir Sin Saltar ">> Nuevo motivo de falla: "
						Leer falla_detalle
						Si falla_detalle <> "" Entonces
							descripcion_orden[posicion_orden] <- falla_detalle
							Escribir "------------------------------------------------------------------"
							Escribir "[OK] Motivo de falla actualizado correctamente."
							Escribir "=================================================================="
						SiNo
							Escribir "  [!] El detalle no puede estar vacío."
						FinSi
						Pausa
					2:
						Escribir ">> Diagnóstico actual: ", diagnostico_orden[posicion_orden]
						Escribir Sin Saltar ">> Nuevo diagnóstico: "
						Leer diag_detalle
						Si diag_detalle <> "" Entonces
							diagnostico_orden[posicion_orden] <- diag_detalle
							Escribir "------------------------------------------------------------------"
							Escribir "[OK] Diagnóstico actualizado correctamente."
							Escribir "=================================================================="
						SiNo
							Escribir "  [!] El diagnóstico no puede estar vacío."
						FinSi
						Pausa
					3:
						Escribir Sin Saltar ">> Código o nombre del repuesto a agregar: "
						Leer nombre_repuesto_modificar
						posicion_repuesto_modificar <- BuscarPosicion(codigo, cantidad_repuestos, nombre_repuesto_modificar)
						Si posicion_repuesto_modificar = -1 Entonces
							posicion_repuesto_modificar <- BuscarPosicion(nombreRepuesto, cantidad_repuestos, nombre_repuesto_modificar)
						FinSi
						
						Si posicion_repuesto_modificar = -1 Entonces
							Escribir "  [!] Repuesto no encontrado en el catálogo."
						SiNo
							Escribir "  Artículo: ", nombreRepuesto[posicion_repuesto_modificar], " | Stock actual: ", stock[posicion_repuesto_modificar], " | Precio: $", precio[posicion_repuesto_modificar]
							Escribir Sin Saltar "  Cantidad requerida: "
							Leer cantidad_repuestos_ot
							Si cantidad_repuestos_ot <= 0 Entonces
								Escribir "  [!] Cantidad inválida (debe ser mayor a cero)."
							SiNo
								Si cantidad_repuestos_ot > stock[posicion_repuesto_modificar] Entonces
									Escribir "  [!] Stock insuficiente en inventario."
								SiNo
									Si cantidad_detalles >= 300 Entonces
										Escribir "  [!] Límite máximo de repuestos asignados a órdenes alcanzado (300)."
									SiNo
										detalle_nro_orden[cantidad_detalles] <- orden_numero[posicion_orden]
										detalle_cod_repuesto[cantidad_detalles] <- codigo[posicion_repuesto_modificar]
										detalle_cantidad[cantidad_detalles] <- cantidad_repuestos_ot
										detalle_precio_unitario[cantidad_detalles] <- precio[posicion_repuesto_modificar]
										cantidad_detalles <- cantidad_detalles + 1
										stock[posicion_repuesto_modificar] <- stock[posicion_repuesto_modificar] - cantidad_repuestos_ot
										Escribir "  [OK] Repuesto agregado y descontado del stock general."
									FinSi
								FinSi
							FinSi
						FinSi
						Pausa
					4:
						tiene_repuestos <- 0
						Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
							Si detalle_nro_orden[i] = orden_numero[posicion_orden] Entonces
								tiene_repuestos <- tiene_repuestos + 1
							FinSi
						FinPara
						
						Si tiene_repuestos = 0 Entonces
							Escribir "  [!] La orden no tiene repuestos asignados."
						SiNo
							Escribir "------------------------------------------------------------------"
							Escribir "Repuestos asignados en la orden:"
							Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
								Si detalle_nro_orden[i] = orden_numero[posicion_orden] Entonces
									Escribir "  - Código: ", detalle_cod_repuesto[i], " (x", detalle_cantidad[i], ")"
								FinSi
							FinPara
							Escribir "------------------------------------------------------------------"
							Escribir Sin Saltar ">> Código de repuesto a quitar: "
							Leer codigo_quitar
							
							posicion_detalle <- -1
							Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
								Si detalle_nro_orden[i] = orden_numero[posicion_orden] Y detalle_cod_repuesto[i] = codigo_quitar Entonces
									posicion_detalle <- i
								FinSi
							FinPara
							
							Si posicion_detalle = -1 Entonces
								Escribir "  [!] El repuesto no esta asignado a esta orden."
							SiNo
								posicion_repuesto_modificar <- BuscarPosicion(codigo, cantidad_repuestos, codigo_quitar)
								Si posicion_repuesto_modificar <> -1 Entonces
									stock[posicion_repuesto_modificar] <- stock[posicion_repuesto_modificar] + detalle_cantidad[posicion_detalle]
								FinSi
								
								Para j <- posicion_detalle Hasta cantidad_detalles - 2 Con Paso 1 Hacer
									detalle_nro_orden[j] <- detalle_nro_orden[j+1]
									detalle_cod_repuesto[j] <- detalle_cod_repuesto[j+1]
									detalle_cantidad[j] <- detalle_cantidad[j+1]
									detalle_precio_unitario[j] <- detalle_precio_unitario[j+1]
								FinPara
								cantidad_detalles <- cantidad_detalles - 1
								Escribir "------------------------------------------------------------------"
								Escribir "[OK] Repuesto quitado de la orden y restaurado al stock."
								Escribir "=================================================================="
							FinSi
						FinSi
						Pausa
					0:
						Escribir "Volviendo al menu de órdenes..."
					De Otro Modo:
						Escribir "Opción no válida."
						Pausa
				FinSegun
			Mientras Que opcion_modificar_ot <> 0
		FinSi
	FinSi
FinFuncion

Funcion BuscarOrden(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles)
	Definir posicion_orden, i Como Entero
	Definir dato_buscar Como Caracter
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                 BUSCAR ORDEN DE TRABAJO                          "
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Patente o número de orden a buscar: "
	Leer dato_buscar
	
	posicion_orden <- BuscarPosicion(patente_orden, cantidad_ordenes, dato_buscar)
	Si posicion_orden = -1 Entonces
		Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
			Si dato_buscar = ConvertirATexto(orden_numero[i]) Entonces
				posicion_orden <- i
			FinSi
		FinPara
	FinSi
	
	Si posicion_orden = -1 Entonces
		Escribir "------------------------------------------------------------------"
		Escribir "  [!] Error: No hay orden registrada para: ", dato_buscar
		Escribir "=================================================================="
		Pausa
	SiNo
		Limpiar Pantalla
		Escribir "+================================================================+"
		Escribir "|                   ORDEN DE TRABAJO Nro ", orden_numero[posicion_orden]
		Escribir "+================================================================+"
		Escribir "| PATENTE:        ", patente_orden[posicion_orden]
		Escribir "| DNI TITULAR:    ", dni_orden[posicion_orden]
		Escribir "| ESTADO ACTUAL:  ", estado_orden[posicion_orden]
		Escribir "| FECHA INGRESO:  ", fecha_inicio[posicion_orden]
		Si fecha_fin[posicion_orden] <> "" Entonces
			Escribir "| FECHA ESTIMADA: ", fecha_fin[posicion_orden]
		FinSi
		Si estado_orden[posicion_orden] = "Finalizada" Entonces
			Escribir "| FECHA ENTREGA:  ", fecha_entrega[posicion_orden]
		FinSi
		Escribir "+----------------------------------------------------------------+"
		Escribir "| MOTIVO INGRESO: ", descripcion_orden[posicion_orden]
		Escribir "| DIAGNÓSTICO:    ", diagnostico_orden[posicion_orden]
		Si trabajo_orden[posicion_orden] <> "" Entonces
			Escribir "| TRABAJO HECHO:  ", trabajo_orden[posicion_orden]
		FinSi
		Escribir "+----------------------------------------------------------------+"
		Escribir "| REPUESTOS UTILIZADOS:"
		Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
			Si detalle_nro_orden[i] = orden_numero[posicion_orden] Entonces
				Escribir "|   - ", detalle_cod_repuesto[i], " (x", detalle_cantidad[i], ") - $", detalle_precio_unitario[i] * detalle_cantidad[i]
			FinSi
		FinPara
		Escribir "+================================================================+"
		Pausa
	FinSi
FinFuncion

Funcion CerrarOrden(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles)
	Definir posicion_orden, i Como Entero
	Definir dato_buscar, confirma_cierre, fecha_ingresada, trabajo_realizado Como Caracter
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                 CERRAR ORDEN DE TRABAJO                          "
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Patente o número de orden a cerrar: "
	Leer dato_buscar
	
	posicion_orden <- BuscarPosicion(patente_orden, cantidad_ordenes, dato_buscar)
	Si posicion_orden = -1 Entonces
		Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
			Si dato_buscar = ConvertirATexto(orden_numero[i]) Entonces
				posicion_orden <- i
			FinSi
		FinPara
	FinSi
	
	Si posicion_orden = -1 Entonces
		Escribir "------------------------------------------------------------------"
		Escribir "  [!] Error: No hay orden registrada para: ", dato_buscar
		Escribir "=================================================================="
		Pausa
	SiNo
		Si estado_orden[posicion_orden] = "Finalizada" Entonces
			Escribir "------------------------------------------------------------------"
			Escribir "  [!] La orden Nro ", orden_numero[posicion_orden], " ya se encuentra Finalizada."
			Escribir "=================================================================="
			Pausa
		SiNo
			Limpiar Pantalla
			Escribir "+================================================================+"
			Escribir "|             RESUMEN DE ORDEN NRO ", orden_numero[posicion_orden]
			Escribir "+================================================================+"
			Escribir "| PATENTE:        ", patente_orden[posicion_orden]
			Escribir "| DNI TITULAR:    ", dni_orden[posicion_orden]
			Escribir "| FECHA INGRESO:  ", fecha_inicio[posicion_orden]
			Si fecha_fin[posicion_orden] <> "" Entonces
				Escribir "| FECHA ESTIMADA: ", fecha_fin[posicion_orden]
			FinSi
			Escribir "| ESTADO ACTUAL:  ", estado_orden[posicion_orden]
			Escribir "| MOTIVO INGRESO: ", descripcion_orden[posicion_orden]
			Escribir "| DIAGNÓSTICO:    ", diagnostico_orden[posicion_orden]
			Escribir "+----------------------------------------------------------------+"
			Escribir "| REPUESTOS UTILIZADOS:"
			Para i <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
				Si detalle_nro_orden[i] = orden_numero[posicion_orden] Entonces
					Escribir "|   - ", detalle_cod_repuesto[i], " (x", detalle_cantidad[i], ") = $", detalle_precio_unitario[i] * detalle_cantidad[i]
				FinSi
			FinPara
			Escribir "+================================================================+"
			
			Escribir Sin Saltar ">> Fecha de finalización / entrega (DD/MM/AAAA): "
			Leer fecha_ingresada
			Si fecha_ingresada = "" Entonces
				fecha_ingresada <- "Hoy"
			FinSi
			
			Escribir Sin Saltar ">> Detalle de trabajo realizado por el mecánico: "
			Leer trabajo_realizado
			
			Escribir "------------------------------------------------------------------"
			Escribir Sin Saltar "Confirmar cierre de la orden? (S/N): "
			Leer confirma_cierre
			
			Si confirma_cierre = "S" o confirma_cierre = "s" Entonces
				estado_orden[posicion_orden] <- "Finalizada"
				fecha_fin[posicion_orden] <- fecha_ingresada
				fecha_entrega[posicion_orden] <- fecha_ingresada
				trabajo_orden[posicion_orden] <- trabajo_realizado
				
				Escribir "=================================================================="
				Escribir "  [OK] Orden cerrada correctamente."
				Escribir "  Nro de orden:   ", orden_numero[posicion_orden]
				Escribir "  Estado nuevo:   ", estado_orden[posicion_orden]
				Escribir "  Fecha entrega:  ", fecha_entrega[posicion_orden]
				Escribir "=================================================================="
				Escribir "  La orden esta lista para facturación (Módulo 5)."
				Escribir "=================================================================="
			SiNo
				Escribir "------------------------------------------------------------------"
				Escribir "  [!] Cierre de orden cancelado."
				Escribir "=================================================================="
			FinSi
			Pausa
		FinSi
	FinSi
FinFuncion

Funcion menuOrdenes(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes Por Referencia, nro_orden_siguiente Por Referencia, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles Por Referencia, dni, nombre, apellido, contadorClientes, patente, marcaVehiculo, modeloVehiculo, dniDueno, contadorVehiculo, codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
	Definir opcion_ordenes Como Entero
	Repetir
		opcion_ordenes <- MostrarMenuOrdenes
		Segun opcion_ordenes Hacer
			1:
				CargarOrden(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, nro_orden_siguiente, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, dni, nombre, apellido, contadorClientes, patente, marcaVehiculo, modeloVehiculo, dniDueno, contadorVehiculo, codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
			2:
				ModificarOrden(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
			3:
				BuscarOrden(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles)
			4:
				CerrarOrden(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles)
			0:
				Escribir "Volviendo al menu principal..."
			De Otro Modo:
				Escribir "Opción no válida."
				Pausa
		FinSegun
	Hasta Que opcion_ordenes = 0
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo repuestos
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuRepuestos
	Limpiar Pantalla
	Definir opcion Como Entero
	Escribir "=================================================================="
	Escribir "                   CONTROL DE REPUESTOS Y STOCK                   "
	Escribir "=================================================================="
	Escribir "  [1] Cargar repuesto nuevo"
	Escribir "  [2] Eliminar repuesto"
	Escribir "  [3] Buscar repuesto en inventario"
	Escribir "  [4] Modificar precio de repuesto"
	Escribir "  [5] Actualizar stock disponible"
	Escribir "  [6] Listado general de catálogo"
	Escribir "  ----------------------------------------------------------------"
	Escribir "  [0] Volver al menu principal"
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese opción deseada: "
	Leer opcion
FinFuncion

Funcion CargarRepuesto(codigo, nombreRepuesto, precio, stock, cantidad_repuestos Por Referencia)
	Definir codNuevo Como Caracter
	Definir posRep Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                    CARGAR REPUESTO AL CATÁLOGO                   "
	Escribir "=================================================================="
	Si cantidad_repuestos >= 100 Entonces
		Escribir "  [!] Capacidad máxima de repuestos alcanzada (límite 100)."
		Pausa
	SiNo
		Escribir ">> Ingrese los datos del repuesto:"
		Escribir ""
		Escribir Sin Saltar "Código de artículo: "
		Leer codNuevo
		posRep <- BuscarPosicion(codigo, cantidad_repuestos, codNuevo)
		Si posRep <> -1 Entonces
			Escribir "------------------------------------------------------------------"
			Escribir "  [!] Error: Ya existe un repuesto con el código ", codNuevo
			Escribir "=================================================================="
			Pausa
		SiNo
			codigo[cantidad_repuestos] <- codNuevo
			Escribir Sin Saltar "Descripción / Nombre del repuesto: "
			Leer nombreRepuesto[cantidad_repuestos]
			
			Repetir
				Escribir Sin Saltar "Precio unitario: $"
				Leer precio[cantidad_repuestos]
				Si precio[cantidad_repuestos] <= 0 Entonces
					Escribir "  [!] El precio debe ser mayor a cero."
				FinSi
			Hasta Que precio[cantidad_repuestos] > 0
			
			Repetir
				Escribir Sin Saltar "Stock inicial disponible: "
				Leer stock[cantidad_repuestos]
				Si stock[cantidad_repuestos] < 0 Entonces
					Escribir "  [!] El stock no puede ser negativo."
				FinSi
			Hasta Que stock[cantidad_repuestos] >= 0
			
			cantidad_repuestos <- cantidad_repuestos + 1
			Escribir "------------------------------------------------------------------"
			Escribir "[OK] Repuesto cargado exitosamente en el catálogo."
			Escribir "=================================================================="
			Pausa
		FinSi
	FinSi
FinFuncion

Funcion EliminarRepuesto(codigo, nombreRepuesto, precio, stock, cantidad_repuestos Por Referencia)
	Definir codBuscar Como Caracter
	Definir posRep, j Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                   ELIMINAR REPUESTO DEL CATÁLOGO                 "
	Escribir "=================================================================="
	Escribir Sin Saltar "Seleccionar código para eliminar: "
	Leer codBuscar
	posRep <- BuscarPosicion(codigo, cantidad_repuestos, codBuscar)
	
	Si posRep <> -1 Entonces
		Para j <- posRep Hasta cantidad_repuestos - 2 Con Paso 1 Hacer
			codigo[j] <- codigo[j+1]
			nombreRepuesto[j] <- nombreRepuesto[j+1]
			precio[j] <- precio[j+1]
			stock[j] <- stock[j+1]
		FinPara
		cantidad_repuestos <- cantidad_repuestos - 1
		Escribir "------------------------------------------------------------------"
		Escribir "[OK] Repuesto eliminado con éxito del inventario."
		Escribir "=================================================================="
	SiNo
		Escribir "------------------------------------------------------------------"
		Escribir "  [!] Error: El código especificado no existe."
		Escribir "=================================================================="
	FinSi
	Pausa
FinFuncion

Funcion BuscarRepuesto(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
	Definir criterio Como Caracter
	Definir posRep Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                   BUSCAR REPUESTO EN CATÁLOGO                    "
	Escribir "=================================================================="
	Escribir Sin Saltar "Ingrese código o nombre del repuesto: "
	Leer criterio
	posRep <- BuscarPosicion(codigo, cantidad_repuestos, criterio)
	Si posRep = -1 Entonces
		posRep <- BuscarPosicion(nombreRepuesto, cantidad_repuestos, criterio)
	FinSi
	
	Si posRep <> -1 Entonces
		Escribir "+----------------------------------------------------------------+"
		Escribir "|                       FICHA DE REPUESTO                        |"
		Escribir "+----------------------------------------------------------------+"
		Escribir "| CÓDIGO:      ", codigo[posRep]
		Escribir "| DESCRIPCIÓN: ", nombreRepuesto[posRep]
		Escribir "| PRECIO UNIT: $", precio[posRep]
		Escribir "| STOCK REAL:  ", stock[posRep], " unidades"
		Escribir "+----------------------------------------------------------------+"
	SiNo
		Escribir "------------------------------------------------------------------"
		Escribir "  [!] Repuesto no encontrado en el catálogo."
		Escribir "=================================================================="
	FinSi
	Pausa
FinFuncion

Funcion ModificarPrecioRepuesto(codigo, nombreRepuesto, precio, cantidad_repuestos)
	Definir codBuscar Como Caracter
	Definir posRep Como Entero
	Definir precioNuevo Como Real
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                    MODIFICAR PRECIO DE REPUESTO                  "
	Escribir "=================================================================="
	Escribir Sin Saltar "Ingrese código del repuesto a modificar: "
	Leer codBuscar
	posRep <- BuscarPosicion(codigo, cantidad_repuestos, codBuscar)
	
	Si posRep <> -1 Entonces
		Escribir ">> Repuesto: ", nombreRepuesto[posRep], " | Precio actual: $", precio[posRep]
		Repetir
			Escribir Sin Saltar "Ingrese el precio nuevo: $"
			Leer precioNuevo
			Si precioNuevo <= 0 Entonces
				Escribir "  [!] El precio debe ser mayor a cero."
			FinSi
		Hasta Que precioNuevo > 0
		precio[posRep] <- precioNuevo
		Escribir "------------------------------------------------------------------"
		Escribir "[OK] Precio actualizado exitosamente."
		Escribir "=================================================================="
	SiNo
		Escribir "------------------------------------------------------------------"
		Escribir "  [!] Error: Repuesto no encontrado."
		Escribir "=================================================================="
	FinSi
	Pausa
FinFuncion

Funcion ModificarStockRepuesto(codigo, nombreRepuesto, stock, cantidad_repuestos)
	Definir codBuscar Como Caracter
	Definir posRep, stockNuevo Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                    ACTUALIZAR STOCK DE REPUESTO                  "
	Escribir "=================================================================="
	Escribir Sin Saltar "Ingrese código del repuesto: "
	Leer codBuscar
	posRep <- BuscarPosicion(codigo, cantidad_repuestos, codBuscar)
	
	Si posRep <> -1 Entonces
		Escribir ">> Repuesto: ", nombreRepuesto[posRep], " | Stock actual: ", stock[posRep]
		Repetir
			Escribir Sin Saltar "Ingrese el nuevo stock disponible: "
			Leer stockNuevo
			Si stockNuevo < 0 Entonces
				Escribir "  [!] El stock no puede ser negativo."
			FinSi
		Hasta Que stockNuevo >= 0
		stock[posRep] <- stockNuevo
		Escribir "------------------------------------------------------------------"
		Escribir "[OK] Stock actualizado exitosamente."
		Escribir "=================================================================="
	SiNo
		Escribir "------------------------------------------------------------------"
		Escribir "  [!] Error: Repuesto no encontrado."
		Escribir "=================================================================="
	FinSi
	Pausa
FinFuncion

Funcion ListarRepuestos(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
	Definir i Como Entero
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                  CATÁLOGO GENERAL DE REPUESTOS                   "
	Escribir "=================================================================="
	Si cantidad_repuestos = 0 Entonces
		Escribir "  [!] No hay repuestos cargados en el inventario."
		Escribir "=================================================================="
	SiNo
		Escribir "Nro | CÓDIGO   | DESCRIPCIÓN                    | PRECIO    | STOCK"
		Escribir "----+----------+--------------------------------+-----------+-------"
		Para i <- 0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
			Escribir i + 1, "   | ", codigo[i], " | ", nombreRepuesto[i], " | $", precio[i], " | ", stock[i]
		FinPara
		Escribir "=================================================================="
		Escribir "Total de artículos en stock: ", cantidad_repuestos
		Escribir "=================================================================="
	FinSi
	Pausa
FinFuncion

Funcion menuRepuestos(codigo, nombreRepuesto, precio, stock, cantidad_repuestos Por Referencia)
	Definir opcion_repuestos Como Entero
	Repetir
		opcion_repuestos <- MostrarMenuRepuestos
		Segun opcion_repuestos Hacer
			1:
				CargarRepuesto(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
			2:
				EliminarRepuesto(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
			3:
				BuscarRepuesto(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
			4:
				ModificarPrecioRepuesto(codigo, nombreRepuesto, precio, cantidad_repuestos)
			5:
				ModificarStockRepuesto(codigo, nombreRepuesto, stock, cantidad_repuestos)
			6:
				ListarRepuestos(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
			0:
				Escribir "Volviendo al menu principal..."
			De Otro Modo:
				Escribir "Opción no válida."
				Pausa
		FinSegun
	Hasta Que opcion_repuestos = 0
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo facturacion
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuFacturacion
	Limpiar Pantalla
	Definir opcion Como Entero
	Escribir "=================================================================="
	Escribir "                      MÓDULO DE FACTURACIÓN                       "
	Escribir "=================================================================="
	Escribir "  [1] Emitir factura de orden finalizada"
	Escribir "  [2] Ver comprobante de factura"
	Escribir "  [3] Listado de facturas emitidas"
	Escribir "  [4] Consultar facturas por DNI de cliente"
	Escribir "  ----------------------------------------------------------------"
	Escribir "  [0] Volver al menu principal"
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese opción deseada: "
	Leer opcion
FinFuncion

Funcion EmitirFactura(orden_numero, dni_orden, patente_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_numero, factura_nro_orden, factura_dni, factura_patente, factura_fecha, factura_repuestos, factura_horas, factura_valor_hora, factura_mano_obra, factura_subtotal, factura_iva, factura_total, cantidad_facturas Por Referencia, nro_factura_siguiente Por Referencia, porcentaje_iva, dni, nombre, apellido, contadorClientes)
	Definir ordenFacturar, posOrden, posCliente, k Como Entero
	Definir totalRepuestos, horasIngresadas, valorHora, manoObra, subtotal, iva, total Como Real
	Definir fechaFactura, confirmaFactura Como Caracter
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                   EMISIÓN DE NUEVA FACTURA                       "
	Escribir "=================================================================="
	Si cantidad_facturas >= 1000 Entonces
		Escribir "  [!] Cantidad máxima de facturas alcanzada (límite 1000)."
		Escribir "=================================================================="
		Pausa
	SiNo
		Escribir Sin Saltar ">> Número de orden a facturar: "
		Leer ordenFacturar
		posOrden <- BuscarPosicion(orden_numero, cantidad_ordenes, ordenFacturar)
		
		Si posOrden = -1 Entonces
			Escribir "------------------------------------------------------------------"
			Escribir "  [!] Error: La orden ingresada no existe."
			Escribir "=================================================================="
			Pausa
		SiNo
			Si estado_orden[posOrden] <> "Finalizada" Entonces
				Escribir "------------------------------------------------------------------"
				Escribir "  [!] La orden todavía está pendiente."
				Escribir "  Debe cerrarse en el Módulo 3 antes de poder emitir la factura."
				Escribir "=================================================================="
				Pausa
			SiNo
				Si BuscarPosicion(factura_nro_orden, cantidad_facturas, ordenFacturar) <> -1 Entonces
					Escribir "------------------------------------------------------------------"
					Escribir "  [!] La orden Nro ", ordenFacturar, " ya fue facturada con anterioridad."
					Escribir "=================================================================="
					Pausa
				SiNo
					posCliente <- BuscarPosicion(dni, contadorClientes, dni_orden[posOrden])
					Escribir "------------------------------------------------------------------"
					Escribir ">> Orden Nro:  ", orden_numero[posOrden]
					Si posCliente <> -1 Entonces
						Escribir ">> Titular:    ", apellido[posCliente], " ", nombre[posCliente]
					FinSi
					Escribir ">> DNI:        ", dni_orden[posOrden]
					Escribir ">> Patente:    ", patente_orden[posOrden]
					Escribir ">> Trabajo:    ", trabajo_orden[posOrden]
					Escribir "------------------------------------------------------------------"
					Escribir "Repuestos utilizados:"
					totalRepuestos <- 0
					Para k <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
						Si detalle_nro_orden[k] = orden_numero[posOrden] Entonces
							Escribir "  - ", detalle_cod_repuesto[k], " x ", detalle_cantidad[k], " = $", detalle_precio_unitario[k] * detalle_cantidad[k]
							totalRepuestos <- totalRepuestos + detalle_precio_unitario[k] * detalle_cantidad[k]
						FinSi
					FinPara
					Si totalRepuestos = 0 Entonces
						Escribir "  (sin repuestos utilizados)"
					FinSi
					Escribir "------------------------------------------------------------------"
					
					Repetir
						Escribir Sin Saltar "Horas de mano de obra trabajadas: "
						Leer horasIngresadas
						Si horasIngresadas <= 0 Entonces
							Escribir "  [!] Las horas deben ser mayores a cero."
						FinSi
					Hasta Que horasIngresadas > 0
					
					Repetir
						Escribir Sin Saltar "Valor por hora de mano de obra: $"
						Leer valorHora
						Si valorHora <= 0 Entonces
							Escribir "  [!] El valor debe ser mayor a cero."
						FinSi
					Hasta Que valorHora > 0
					
					Escribir Sin Saltar "Fecha de emisión (DD/MM/AAAA): "
					Leer fechaFactura
					
					manoObra <- horasIngresadas * valorHora
					subtotal <- totalRepuestos + manoObra
					iva <- redon(subtotal * porcentaje_iva) / 100
					total <- subtotal + iva
					
					Escribir "+================================================================+"
					Escribir "|                   PRELIQUIDACIÓN DE FACTURA                    |"
					Escribir "+================================================================+"
					Escribir "| Subtotal Repuestos:           $", totalRepuestos
					Escribir "| Mano de Obra (", horasIngresadas, " hs x $", valorHora, "):  $", manoObra
					Escribir "| Subtotal Gravado:             $", subtotal
					Escribir "| IVA (", porcentaje_iva, "%):                    $", iva
					Escribir "+----------------------------------------------------------------+"
					Escribir "| TOTAL A PAGAR:                $", total
					Escribir "+================================================================+"
					Escribir Sin Saltar "Confirmar emisión definitiva? (S/N): "
					Leer confirmaFactura
					
					Si confirmaFactura = "S" o confirmaFactura = "s" Entonces
						factura_numero[cantidad_facturas] <- nro_factura_siguiente
						factura_nro_orden[cantidad_facturas] <- orden_numero[posOrden]
						factura_dni[cantidad_facturas] <- dni_orden[posOrden]
						factura_patente[cantidad_facturas] <- patente_orden[posOrden]
						factura_fecha[cantidad_facturas] <- fechaFactura
						factura_repuestos[cantidad_facturas] <- totalRepuestos
						factura_horas[cantidad_facturas] <- horasIngresadas
						factura_valor_hora[cantidad_facturas] <- valorHora
						factura_mano_obra[cantidad_facturas] <- manoObra
						factura_subtotal[cantidad_facturas] <- subtotal
						factura_iva[cantidad_facturas] <- iva
						factura_total[cantidad_facturas] <- total
						cantidad_facturas <- cantidad_facturas + 1
						nro_factura_siguiente <- nro_factura_siguiente + 1
						Escribir "------------------------------------------------------------------"
						Escribir "[OK] Factura Nro ", factura_numero[cantidad_facturas - 1], " emitida correctamente."
						Escribir "=================================================================="
					SiNo
						Escribir "------------------------------------------------------------------"
						Escribir "  [!] Emisión de factura cancelada."
						Escribir "=================================================================="
					FinSi
					Pausa
				FinSi
			FinSi
		FinSi
	FinSi
FinFuncion

Funcion VerComprobanteFactura(factura_numero, factura_nro_orden, factura_dni, factura_patente, factura_fecha, factura_repuestos, factura_horas, factura_valor_hora, factura_mano_obra, factura_subtotal, factura_iva, factura_total, cantidad_facturas, porcentaje_iva, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, dni, nombre, apellido, contadorClientes)
	Definir facturaBuscar, posFactura, posCliente, k Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                CONSULTAR COMPROBANTE DE FACTURA                  "
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese número de factura: "
	Leer facturaBuscar
	posFactura <- BuscarPosicion(factura_numero, cantidad_facturas, facturaBuscar)
	
	Si posFactura = -1 Entonces
		Escribir "------------------------------------------------------------------"
		Escribir "  [!] Error: La factura Nro ", facturaBuscar, " no existe."
		Escribir "=================================================================="
	SiNo
		posCliente <- BuscarPosicion(dni, contadorClientes, factura_dni[posFactura])
		Limpiar Pantalla
		Escribir "+================================================================+"
		Escribir "|                    FIXLY - TALLER MECÁNICO                     |"
		Escribir "|                  COMPROBANTE DE FACTURACIÓN                    |"
		Escribir "+================================================================+"
		Escribir "| FACTURA Nro: ", factura_numero[posFactura], "                   FECHA: ", factura_fecha[posFactura]
		Escribir "| ORDEN ASOCIADA: Nro ", factura_nro_orden[posFactura], "         PATENTE: ", factura_patente[posFactura]
		Si posCliente <> -1 Entonces
			Escribir "| TITULAR: ", apellido[posCliente], " ", nombre[posCliente], " (DNI: ", factura_dni[posFactura], ")"
		SiNo
			Escribir "| TITULAR DNI: ", factura_dni[posFactura]
		FinSi
		Escribir "+----------------------------------------------------------------+"
		Escribir "| DETALLE DE REPUESTOS Y MATERIALES:                             |"
		Para k <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
			Si detalle_nro_orden[k] = factura_nro_orden[posFactura] Entonces
				Escribir "|   - ", detalle_cod_repuesto[k], " x ", detalle_cantidad[k], " = $", detalle_precio_unitario[k] * detalle_cantidad[k]
			FinSi
		FinPara
		Escribir "+----------------------------------------------------------------+"
		Escribir "| LIQUIDACIÓN DE COSTOS:                                         |"
		Escribir "|   Subtotal Repuestos:           $", factura_repuestos[posFactura]
		Escribir "|   Mano de Obra (", factura_horas[posFactura], " hs x $", factura_valor_hora[posFactura], "): $", factura_mano_obra[posFactura]
		Escribir "|   Subtotal Gravado:             $", factura_subtotal[posFactura]
		Escribir "|   IVA (", porcentaje_iva, "%):                    $", factura_iva[posFactura]
		Escribir "+----------------------------------------------------------------+"
		Escribir "| TOTAL DE FACTURA:               $", factura_total[posFactura]
		Escribir "+================================================================+"
	FinSi
	Pausa
FinFuncion

Funcion ListarFacturas(factura_numero, factura_nro_orden, factura_fecha, factura_patente, factura_total, cantidad_facturas)
	Definir i Como Entero
	Definir totalFacturado Como Real
	
	Limpiar Pantalla
	Escribir "=================================================================================="
	Escribir "                            LISTADO DE FACTURAS EMITIDAS                          "
	Escribir "=================================================================================="
	Si cantidad_facturas = 0 Entonces
		Escribir "  [!] No hay facturas emitidas en el sistema."
		Escribir "=================================================================================="
	SiNo
		totalFacturado <- 0
		Escribir "NRO | ORDEN | FECHA      | PATENTE  | TOTAL FACTURADO"
		Escribir "----+-------+------------+----------+---------------------------------------------"
		Para i <- 0 Hasta cantidad_facturas - 1 Con Paso 1 Hacer
			Escribir factura_numero[i], "   | ", factura_nro_orden[i], "     | ", factura_fecha[i], " | ", factura_patente[i], "  | $", factura_total[i]
			totalFacturado <- totalFacturado + factura_total[i]
		FinPara
		Escribir "=================================================================================="
		Escribir "Facturas emitidas: ", cantidad_facturas, " | Total general facturado: $", totalFacturado
		Escribir "=================================================================================="
	FinSi
	Pausa
FinFuncion

Funcion ConsultarFacturasDni(factura_numero, factura_nro_orden, factura_dni, factura_fecha, factura_patente, factura_total, cantidad_facturas)
	Definir dniBuscar, i, facturasEncontradas Como Entero
	Definir totalFacturado Como Real
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                 FACTURAS POR CLIENTE (DNI)                       "
	Escribir "=================================================================="
	dniBuscar <- LeerDni(">> DNI del cliente a consultar: ")
	facturasEncontradas <- 0
	totalFacturado <- 0
	
	Para i <- 0 Hasta cantidad_facturas - 1 Con Paso 1 Hacer
		Si factura_dni[i] = dniBuscar Entonces
			Si facturasEncontradas = 0 Entonces
				Escribir "------------------------------------------------------------------"
				Escribir "NRO | ORDEN | FECHA      | PATENTE  | TOTAL FACTURADO"
				Escribir "----+-------+------------+----------+-----------------------------"
			FinSi
			Escribir factura_numero[i], "   | ", factura_nro_orden[i], "     | ", factura_fecha[i], " | ", factura_patente[i], "  | $", factura_total[i]
			facturasEncontradas <- facturasEncontradas + 1
			totalFacturado <- totalFacturado + factura_total[i]
		FinSi
	FinPara
	
	Si facturasEncontradas = 0 Entonces
		Escribir "  [!] No se registran facturas emitidas para el DNI ", dniBuscar
	SiNo
		Escribir "=================================================================="
		Escribir "Total de comprobantes: ", facturasEncontradas, " | Monto acumulado: $", totalFacturado
	FinSi
	Escribir "=================================================================="
	Pausa
FinFuncion

Funcion menuFacturacion(orden_numero, dni_orden, patente_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_numero, factura_nro_orden, factura_dni, factura_patente, factura_fecha, factura_repuestos, factura_horas, factura_valor_hora, factura_mano_obra, factura_subtotal, factura_iva, factura_total, cantidad_facturas Por Referencia, nro_factura_siguiente Por Referencia, porcentaje_iva, dni, nombre, apellido, contadorClientes)
	Definir opcion_facturacion Como Entero
	Repetir
		opcion_facturacion <- MostrarMenuFacturacion
		Segun opcion_facturacion Hacer
			1:
				EmitirFactura(orden_numero, dni_orden, patente_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_numero, factura_nro_orden, factura_dni, factura_patente, factura_fecha, factura_repuestos, factura_horas, factura_valor_hora, factura_mano_obra, factura_subtotal, factura_iva, factura_total, cantidad_facturas, nro_factura_siguiente, porcentaje_iva, dni, nombre, apellido, contadorClientes)
			2:
				VerComprobanteFactura(factura_numero, factura_nro_orden, factura_dni, factura_patente, factura_fecha, factura_repuestos, factura_horas, factura_valor_hora, factura_mano_obra, factura_subtotal, factura_iva, factura_total, cantidad_facturas, porcentaje_iva, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, dni, nombre, apellido, contadorClientes)
			3:
				ListarFacturas(factura_numero, factura_nro_orden, factura_fecha, factura_patente, factura_total, cantidad_facturas)
			4:
				ConsultarFacturasDni(factura_numero, factura_nro_orden, factura_dni, factura_fecha, factura_patente, factura_total, cantidad_facturas)
			0:
				Escribir "Volviendo al menu principal..."
			De Otro Modo:
				Escribir "Opción no valida."
				Pausa
		FinSegun
	Hasta Que opcion_facturacion = 0
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo historial de reparaciones
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuHistorial
	Limpiar Pantalla
	Definir opcion Como Entero
	Escribir "=================================================================="
	Escribir "                    HISTORIAL DE REPARACIONES                     "
	Escribir "=================================================================="
	Escribir "  [1] Consultar historial por patente"
	Escribir "  [2] Consultar historial por DNI del cliente"
	Escribir "  [3] Listado general de reparaciones finalizadas"
	Escribir "  ----------------------------------------------------------------"
	Escribir "  [0] Volver al menu principal"
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese opción deseada: "
	Leer opcion
FinFuncion

Funcion ConsultarHistorialPatente(patente_orden, orden_numero, dni_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_nro_orden, factura_numero, factura_total, cantidad_facturas)
	Definir patBuscar Como Caracter
	Definir i, k, posFactura, ordenesEncontradas Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "               HISTORIAL DE REPARACIÓN POR PATENTE                "
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese patente a consultar: "
	Leer patBuscar
	ordenesEncontradas <- 0
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "       HISTORIAL DE REPARACIONES - PATENTE: ", patBuscar
	Escribir "=================================================================="
	
	Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
		Si patente_orden[i] = patBuscar Y estado_orden[i] = "Finalizada" Entonces
			ordenesEncontradas <- ordenesEncontradas + 1
			Escribir "+----------------------------------------------------------------+"
			Escribir "| ORDEN NRO:      ", orden_numero[i], " | ESTADO: FINALIZADA"
			Escribir "| FECHA INGRESO:  ", fecha_inicio[i], " | FECHA ENTREGA: ", fecha_entrega[i]
			Escribir "| DNI TITULAR:    ", dni_orden[i]
			Escribir "| MOTIVO INGRESO: ", descripcion_orden[i]
			Escribir "| DIAGNÓSTICO:    ", diagnostico_orden[i]
			Escribir "| TRABAJO HECHO:  ", trabajo_orden[i]
			Escribir "| Repuestos utilizados:"
			Para k <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
				Si detalle_nro_orden[k] = orden_numero[i] Entonces
					Escribir "|   * ", detalle_cod_repuesto[k], " (x", detalle_cantidad[k], ") - $", detalle_precio_unitario[k] * detalle_cantidad[k]
				FinSi
			FinPara
			
			posFactura <- BuscarPosicion(factura_nro_orden, cantidad_facturas, orden_numero[i])
			Si posFactura <> -1 Entonces
				Escribir "| FACTURA ASOCIADA: Factura Nro ", factura_numero[posFactura], " | Total: $", factura_total[posFactura]
			SiNo
				Escribir "| FACTURA ASOCIADA: Pendiente de emisión"
			FinSi
			Escribir "+----------------------------------------------------------------+"
		FinSi
	FinPara
	
	Si ordenesEncontradas = 0 Entonces
		Escribir "  [!] No se registran reparaciones finalizadas para la patente ", patBuscar
	SiNo
		Escribir "Total de reparaciones finalizadas encontradas: ", ordenesEncontradas
	FinSi
	Escribir "=================================================================="
	Pausa
FinFuncion

Funcion ConsultarHistorialDni(dni_orden, patente_orden, orden_numero, fecha_inicio, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, factura_nro_orden, factura_numero, factura_total, cantidad_facturas, dni, nombre, apellido, contadorClientes)
	Definir dniBuscar, i, posCliente, posFactura, ordenesEncontradas Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "             HISTORIAL DE REPARACIÓN POR CLIENTE                  "
	Escribir "=================================================================="
	dniBuscar <- LeerDni(">> Ingrese DNI del cliente a consultar: ")
	posCliente <- BuscarPosicion(dni, contadorClientes, dniBuscar)
	ordenesEncontradas <- 0
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Si posCliente <> -1 Entonces
		Escribir " HISTORIAL: ", apellido[posCliente], " ", nombre[posCliente], " (DNI: ", dniBuscar, ")"
	SiNo
		Escribir " HISTORIAL: DNI ", dniBuscar
	FinSi
	Escribir "=================================================================="
	
	Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
		Si dni_orden[i] = dniBuscar Y estado_orden[i] = "Finalizada" Entonces
			ordenesEncontradas <- ordenesEncontradas + 1
			Escribir "+----------------------------------------------------------------+"
			Escribir "| ORDEN NRO:     ", orden_numero[i], " | PATENTE: ", patente_orden[i]
			Escribir "| FECHA INGRESO: ", fecha_inicio[i], " | FECHA ENTREGA: ", fecha_entrega[i]
			Escribir "| MOTIVO/FALLA:  ", descripcion_orden[i]
			Escribir "| TRABAJO HECHO: ", trabajo_orden[i]
			
			posFactura <- BuscarPosicion(factura_nro_orden, cantidad_facturas, orden_numero[i])
			Si posFactura <> -1 Entonces
				Escribir "| FACTURA: Nro ", factura_numero[posFactura], " | Total: $", factura_total[posFactura]
			SiNo
				Escribir "| FACTURA: Pendiente de cobro"
			FinSi
			Escribir "+----------------------------------------------------------------+"
		FinSi
	FinPara
	
	Si ordenesEncontradas = 0 Entonces
		Escribir "  [!] No se registran reparaciones finalizadas para el DNI ", dniBuscar
	SiNo
		Escribir "Total de reparaciones del cliente: ", ordenesEncontradas
	FinSi
	Escribir "=================================================================="
	Pausa
FinFuncion

Funcion ListarHistorialGeneral(orden_numero, patente_orden, dni_orden, fecha_inicio, fecha_entrega, trabajo_orden, estado_orden, cantidad_ordenes, factura_nro_orden, factura_total, cantidad_facturas)
	Definir i, posFactura, totalFinalizadas Como Entero
	Limpiar Pantalla
	Escribir "=================================================================================="
	Escribir "                   LISTADO GENERAL DE REPARACIONES FINALIZADAS                    "
	Escribir "=================================================================================="
	totalFinalizadas <- 0
	Escribir "ORDEN | PATENTE  | DNI DUEÑO | INGRESO    | ENTREGA    | TOTAL FACTURADO"
	Escribir "------+----------+-----------+------------+------------+--------------------------"
	Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
		Si estado_orden[i] = "Finalizada" Entonces
			totalFinalizadas <- totalFinalizadas + 1
			posFactura <- BuscarPosicion(factura_nro_orden, cantidad_facturas, orden_numero[i])
			Si posFactura <> -1 Entonces
				Escribir orden_numero[i], "     | ", patente_orden[i], "  | ", dni_orden[i], "  | ", fecha_inicio[i], " | ", fecha_entrega[i], " | $", factura_total[posFactura]
			SiNo
				Escribir orden_numero[i], "     | ", patente_orden[i], "  | ", dni_orden[i], "  | ", fecha_inicio[i], " | ", fecha_entrega[i], " | (Pendiente)"
			FinSi
		FinSi
	FinPara
	
	Si totalFinalizadas = 0 Entonces
		Escribir "  [!] No hay reparaciones finalizadas registradas."
	SiNo
		Escribir "=================================================================================="
		Escribir "Total de reparaciones finalizadas en el taller: ", totalFinalizadas
	FinSi
	Escribir "=================================================================================="
	Pausa
FinFuncion

Funcion menuHistorial(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_nro_orden, factura_numero, factura_total, cantidad_facturas, dni, nombre, apellido, contadorClientes)
	Definir opcion_historial Como Entero
	Repetir
		opcion_historial <- MostrarMenuHistorial
		Segun opcion_historial Hacer
			1:
				ConsultarHistorialPatente(patente_orden, orden_numero, dni_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_nro_orden, factura_numero, factura_total, cantidad_facturas)
			2:
				ConsultarHistorialDni(dni_orden, patente_orden, orden_numero, fecha_inicio, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, factura_nro_orden, factura_numero, factura_total, cantidad_facturas, dni, nombre, apellido, contadorClientes)
			3:
				ListarHistorialGeneral(orden_numero, patente_orden, dni_orden, fecha_inicio, fecha_entrega, trabajo_orden, estado_orden, cantidad_ordenes, factura_nro_orden, factura_total, cantidad_facturas)
			0:
				Escribir "Volviendo al menu principal..."
			De Otro Modo:
				Escribir "Opción no válida."
				Pausa
		FinSegun
	Hasta Que opcion_historial = 0
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo usuarios y sesiones
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuUsuarios
	Limpiar Pantalla
	Definir opcion Como Entero
	Escribir "=================================================================="
	Escribir "                    GESTIÓN DE USUARIOS Y ROLES                   "
	Escribir "=================================================================="
	Escribir "  [1] Ver usuarios registrados"
	Escribir "  [2] Registrar nuevo usuario"
	Escribir "  ----------------------------------------------------------------"
	Escribir "  [0] Volver al menu principal"
	Escribir "=================================================================="
	Escribir Sin Saltar ">> Ingrese opción deseada: "
	Leer opcion
FinFuncion

Funcion ListarUsuarios(usuario_nombre, usuario_rol, cantidad_usuarios)
	Definir i Como Entero
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                      USUARIOS REGISTRADOS                        "
	Escribir "=================================================================="
	Escribir "Nro | NOMBRE DE USUARIO     | ROL ASIGNADO"
	Escribir "----+-----------------------+-------------------------------------"
	Para i <- 0 Hasta cantidad_usuarios - 1 Con Paso 1 Hacer
		Escribir i + 1, "   | ", usuario_nombre[i], " | ", usuario_rol[i]
	FinPara
	Escribir "=================================================================="
	Escribir "Total de cuentas activas: ", cantidad_usuarios
	Escribir "=================================================================="
	Pausa
FinFuncion

Funcion RegistrarUsuario(usuario_nombre, usuario_clave, usuario_rol, cantidad_usuarios Por Referencia, rol_activo)
	Definir nuevoUser, nuevaPass Como Caracter
	Definir opcRol, posUser Como Entero
	
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "                    REGISTRAR NUEVO USUARIO                       "
	Escribir "=================================================================="
	Si rol_activo <> "administrador" Entonces
		Escribir "  [!] Acceso denegado: solo un administrador puede registrar usuarios."
		Escribir "=================================================================="
		Pausa
	SiNo
		Si cantidad_usuarios >= 20 Entonces
			Escribir "  [!] Límite de usuarios alcanzado (máximo 20 cuentas)."
			Escribir "=================================================================="
			Pausa
		SiNo
			Escribir Sin Saltar ">> Nombre de usuario nuevo: "
			Leer nuevoUser
			posUser <- BuscarPosicion(usuario_nombre, cantidad_usuarios, nuevoUser)
			Si posUser <> -1 Entonces
				Escribir "------------------------------------------------------------------"
				Escribir "  [!] Error: El nombre de usuario ya existe en el sistema."
				Escribir "=================================================================="
				Pausa
			SiNo
				usuario_nombre[cantidad_usuarios] <- nuevoUser
				Escribir Sin Saltar ">> Contraseña de acceso: "
				Leer usuario_clave[cantidad_usuarios]
				
				Repetir
					Escribir "------------------------------------------------------------------"
					Escribir "Seleccione rol para el nuevo usuario:"
					Escribir "  [1] Administrador (Acceso total al sistema)"
					Escribir "  [2] Mecanico (Ordenes, Repuestos, Historial)"
					Escribir "  [3] Cliente (Historial de Reparaciones)"
					Escribir Sin Saltar ">> Opción: "
					Leer opcRol
				Hasta Que opcRol >= 1 Y opcRol <= 3
				
				Segun opcRol Hacer
					1:
						usuario_rol[cantidad_usuarios] <- "administrador"
					2:
						usuario_rol[cantidad_usuarios] <- "mecanico"
					3:
						usuario_rol[cantidad_usuarios] <- "cliente"
				FinSegun
				
				cantidad_usuarios <- cantidad_usuarios + 1
				Escribir "------------------------------------------------------------------"
				Escribir "[OK] Usuario registrado exitosamente en el sistema."
				Escribir "=================================================================="
				Pausa
			FinSi
		FinSi
	FinSi
FinFuncion

Funcion menuUsuarios(usuario_nombre, usuario_clave, usuario_rol, cantidad_usuarios Por Referencia, rol_activo)
	Definir opcion_usuarios Como Entero
	Repetir
		opcion_usuarios <- MostrarMenuUsuarios
		Segun opcion_usuarios Hacer
			1:
				ListarUsuarios(usuario_nombre, usuario_rol, cantidad_usuarios)
			2:
				RegistrarUsuario(usuario_nombre, usuario_clave, usuario_rol, cantidad_usuarios, rol_activo)
			0:
				Escribir "Volviendo al menu principal..."
			De Otro Modo:
				Escribir "Opción no valida."
				Pausa
		FinSegun
	Hasta Que opcion_usuarios = 0
FinFuncion

Funcion CerrarSesion(usuario_activo Por Referencia, rol_activo Por Referencia)
	Limpiar Pantalla
	Escribir "=================================================================="
	Escribir "  [OK] Sesión de ", usuario_activo, " cerrada correctamente."
	Escribir "  Volviendo a la selección de tipo de usuario..."
	Escribir "=================================================================="
	usuario_activo <- ""
	rol_activo <- ""
	Pausa
FinFuncion


// =====================================================================
// algoritmo principal
// =====================================================================
Algoritmo Fixly
	
	// variables de sesion y usuarios
	Definir tipoUsuario, opcion_menu, cantidad_usuarios Como Entero
	Definir usuario_nombre, usuario_clave, usuario_rol Como Caracter
	Definir usuario_activo, rol_activo, rol_elegido Como Caracter
	Definir intentos, posUser Como Entero
	Definir userIngresado, passIngresada Como Caracter
	Dimension usuario_nombre[20]
	Dimension usuario_clave[20]
	Dimension usuario_rol[20]
	usuario_activo <- ""
	rol_activo <- ""
	
	// variables de clientes
	Definir contadorClientes, dni Como Entero
	Definir nombre, apellido, celular Como Caracter
	Dimension nombre[1000], apellido[1000], celular[1000], dni[1000]
	
	// variables de vehiculos
	Definir contadorVehiculo, dniDueno Como Entero
	Definir marcaVehiculo, modeloVehiculo, patente Como Caracter
	Dimension marcaVehiculo[1000], modeloVehiculo[1000], patente[1000], dniDueno[1000]
	
	// variables de repuestos
	Definir cantidad_repuestos, stock Como Entero
	Definir codigo, nombreRepuesto Como Caracter
	Definir precio Como Real
	Dimension codigo[100], nombreRepuesto[100], precio[100], stock[100]
	
	// variables de ordenes de trabajo
	Definir cantidad_ordenes, cantidad_detalles, nro_orden_siguiente Como Entero
	Definir orden_numero, dni_orden, detalle_nro_orden, detalle_cantidad Como Entero
	Definir patente_orden, fecha_inicio, fecha_fin, fecha_entrega, estado_orden Como Caracter
	Definir descripcion_orden, diagnostico_orden, trabajo_orden, detalle_cod_repuesto Como Caracter
	Definir detalle_precio_unitario Como Real
	Dimension orden_numero[1000], dni_orden[1000], patente_orden[1000], estado_orden[1000]
	Dimension fecha_inicio[1000], fecha_fin[1000], fecha_entrega[1000]
	Dimension descripcion_orden[1000], diagnostico_orden[1000], trabajo_orden[1000]
	Dimension detalle_nro_orden[300], detalle_cod_repuesto[300], detalle_cantidad[300], detalle_precio_unitario[300]
	
	// variables de facturacion
	Definir cantidad_facturas, nro_factura_siguiente Como Entero
	Definir factura_numero, factura_nro_orden, factura_dni Como Entero
	Definir factura_patente, factura_fecha Como Caracter
	Definir factura_repuestos, factura_horas, factura_valor_hora, factura_mano_obra, factura_subtotal, factura_iva, factura_total Como Real
	Definir porcentaje_iva Como Real
	Dimension factura_numero[1000], factura_nro_orden[1000], factura_dni[1000], factura_patente[1000], factura_fecha[1000]
	Dimension factura_repuestos[1000], factura_horas[1000], factura_valor_hora[1000], factura_mano_obra[1000]
	Dimension factura_subtotal[1000], factura_iva[1000], factura_total[1000]
	porcentaje_iva <- 21
	
	// --- carga de datos de prueba integrados ---
	// 1. usuarios
	cantidad_usuarios <- 3
	usuario_nombre[0] <- "admin"
	usuario_clave[0] <- "admin123"
	usuario_rol[0] <- "administrador"
	
	usuario_nombre[1] <- "mecanico"
	usuario_clave[1] <- "meca123"
	usuario_rol[1] <- "mecanico"
	
	usuario_nombre[2] <- "cliente"
	usuario_clave[2] <- "cliente123"
	usuario_rol[2] <- "cliente"
	
	// 2. clientes
	contadorClientes <- 5
	nombre[0] <- "Carlos"
	apellido[0] <- "Gomez"
	celular[0] <- "1144556677"
	dni[0] <- 30111222
	
	nombre[1] <- "Mariana"
	apellido[1] <- "Lopez"
	celular[1] <- "1155667788"
	dni[1] <- 28555666
	
	nombre[2] <- "Jorge"
	apellido[2] <- "Fernandez"
	celular[2] <- "1166778899"
	dni[2] <- 35222111
	
	nombre[3] <- "Lucia"
	apellido[3] <- "Diaz"
	celular[3] <- "1177889900"
	dni[3] <- 40333444
	
	nombre[4] <- "Martin"
	apellido[4] <- "Alvarez"
	celular[4] <- "1188990011"
	dni[4] <- 27888999
	
	// 3. vehiculos
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
	
	// 4. repuestos con stock
	cantidad_repuestos <- 5
	codigo[0] <- "ACE-01"
	nombreRepuesto[0] <- "Aceite Sintetico 5W30"
	precio[0] <- 35000
	stock[0] <- 20
	
	codigo[1] <- "FIL-01"
	nombreRepuesto[1] <- "Filtro de Aceite"
	precio[1] <- 12000
	stock[1] <- 15
	
	codigo[2] <- "PAS-01"
	nombreRepuesto[2] <- "Pastillas de Freno Delanteras"
	precio[2] <- 48000
	stock[2] <- 10
	
	codigo[3] <- "BUJ-01"
	nombreRepuesto[3] <- "Juego de Bujias"
	precio[3] <- 28000
	stock[3] <- 12
	
	codigo[4] <- "COR-01"
	nombreRepuesto[4] <- "Correa de Distribucion"
	precio[4] <- 65000
	stock[4] <- 8
	
	// 5. ordenes y facturacion precargada
	cantidad_ordenes <- 2
	nro_orden_siguiente <- 3
	
	orden_numero[0] <- 1
	dni_orden[0] <- 30111222
	patente_orden[0] <- "AB123CD"
	fecha_inicio[0] <- "01/10/2026"
	fecha_fin[0] <- "03/10/2026"
	fecha_entrega[0] <- "03/10/2026"
	descripcion_orden[0] <- "Service programado y cambio de fluidos"
	diagnostico_orden[0] <- "Mantenimiento preventivo general"
	trabajo_orden[0] <- "Cambio de aceite y filtro realizado"
	estado_orden[0] <- "Finalizada"
	
	cantidad_detalles <- 2
	detalle_nro_orden[0] <- 1
	detalle_cod_repuesto[0] <- "ACE-01"
	detalle_cantidad[0] <- 1
	detalle_precio_unitario[0] <- 35000
	
	detalle_nro_orden[1] <- 1
	detalle_cod_repuesto[1] <- "FIL-01"
	detalle_cantidad[1] <- 1
	detalle_precio_unitario[1] <- 12000
	
	cantidad_facturas <- 1
	nro_factura_siguiente <- 2
	factura_numero[0] <- 1
	factura_nro_orden[0] <- 1
	factura_dni[0] <- 30111222
	factura_patente[0] <- "AB123CD"
	factura_fecha[0] <- "03/10/2026"
	factura_repuestos[0] <- 47000
	factura_horas[0] <- 2
	factura_valor_hora[0] <- 15000
	factura_mano_obra[0] <- 30000
	factura_subtotal[0] <- 77000
	factura_iva[0] <- 16170
	factura_total[0] <- 93170
	
	orden_numero[1] <- 2
	dni_orden[1] <- 28555666
	patente_orden[1] <- "AC456EF"
	fecha_inicio[1] <- "10/10/2026"
	fecha_fin[1] <- ""
	fecha_entrega[1] <- ""
	descripcion_orden[1] <- "Ruido al frenar en rueda delantera"
	diagnostico_orden[1] <- "Desgaste de pastillas de freno"
	trabajo_orden[1] <- ""
	estado_orden[1] <- "Pendiente"
	// --- fin de carga de datos de prueba ---
	
	Limpiar Pantalla
	MostrarLogo
	Escribir "  Sistema Integral de Gestion de Taller Mecánico"
	Escribir "  Laboratorio de Computación | Versión 2.0"
	Pausa
	
	Repetir
		// 1. seleccion del tipo de usuario
		Repetir
			Limpiar Pantalla
			MostrarLogo
			Escribir "                 SELECCIONE EL TIPO DE USUARIO                    "
			Escribir "=================================================================="
			Escribir "  [1] Administrador"
			Escribir "  [2] Mecanico"
			Escribir "  [3] Cliente"
			Escribir "  ----------------------------------------------------------------"
			Escribir "  [0] Salir del Sistema"
			Escribir "=================================================================="
			Escribir Sin Saltar ">> Ingrese opción deseada: "
			Leer tipoUsuario
			Si tipoUsuario < 0 o tipoUsuario > 3 Entonces
				Escribir "  [!] Opción no válida."
				Pausa
			FinSi
		Hasta Que tipoUsuario >= 0 Y tipoUsuario <= 3
		
		Si tipoUsuario <> 0 Entonces
			// 2. inicio de sesion (3 intentos)
			Segun tipoUsuario Hacer
				1:
					rol_elegido <- "administrador"
				2:
					rol_elegido <- "mecanico"
				3:
					rol_elegido <- "cliente"
			FinSegun
			usuario_activo <- ""
			rol_activo <- ""
			intentos <- 0
			Repetir
				intentos <- intentos + 1
				Limpiar Pantalla
				Escribir "=================================================================="
				Escribir "                     INICIAR SESIÓN EN FIXLY                      "
				Escribir "=================================================================="
				Escribir "  Tipo de usuario: ", rol_elegido, " | Intento ", intentos, " de 3"
				Escribir "------------------------------------------------------------------"
				Escribir Sin Saltar ">> Nombre de usuario: "
				Leer userIngresado
				posUser <- BuscarPosicion(usuario_nombre, cantidad_usuarios, userIngresado)
				Si posUser = -1 Entonces
					Escribir "------------------------------------------------------------------"
					Escribir "  [!] Error: Usuario no registrado."
					Escribir "=================================================================="
					Pausa
				SiNo
					Escribir Sin Saltar ">> Contraseña: "
					Leer passIngresada
					Si passIngresada <> usuario_clave[posUser] Entonces
						Escribir "------------------------------------------------------------------"
						Escribir "  [!] Error: Contraseña incorrecta."
						Escribir "=================================================================="
					SiNo
						Si usuario_rol[posUser] <> rol_elegido Entonces
							Escribir "------------------------------------------------------------------"
							Escribir "  [!] Error: El usuario no corresponde al tipo seleccionado."
							Escribir "=================================================================="
						SiNo
							usuario_activo <- usuario_nombre[posUser]
							rol_activo <- usuario_rol[posUser]
							Escribir "------------------------------------------------------------------"
							Escribir "[OK] Sesión iniciada correctamente."
							Escribir "Bienvenido/a ", usuario_activo, "! Rol activo: ", rol_activo
							Escribir "=================================================================="
						FinSi
					FinSi
					Pausa
				FinSi
			Hasta Que usuario_activo <> "" O intentos = 3
			
			// 3. si el ingreso es correcto, se abren los modulos segun el tipo de usuario
			Si usuario_activo = "" Entonces
				Limpiar Pantalla
				Escribir "=================================================================="
				Escribir "  [!] Se superaron los 3 intentos permitidos."
				Escribir "  Volviendo a la selección de tipo de usuario..."
				Escribir "=================================================================="
				Pausa
			SiNo
				Segun tipoUsuario Hacer
					1:
						// ------------------------------------------------
						// administrador - todos los modulos
						// ------------------------------------------------
						Repetir
							Limpiar Pantalla
							MostrarLogo
							Escribir "  >> USUARIO: ", usuario_activo, " | ROL: ", rol_activo
							Escribir "=================================================================="
							Escribir "  [1] Módulo Clientes              [5] Módulo Facturación"
							Escribir "  [2] Módulo Vehículos             [6] Historial de Reparaciones"
							Escribir "  [3] Ordenes de Trabajo           [7] Usuarios y Sesiones"
							Escribir "  [4] Repuestos y Stock"
							Escribir "  ----------------------------------------------------------------"
							Escribir "  [0] Cerrar Sesion"
							Escribir "=================================================================="
							Escribir Sin Saltar ">> Ingrese opción deseada: "
							Leer opcion_menu
							Segun opcion_menu Hacer
								1:
									menuClientes(nombre, apellido, celular, dni, contadorClientes)
								2:
									menuVehiculos(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, celular, contadorClientes)
								3:
									menuOrdenes(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, nro_orden_siguiente, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, dni, nombre, apellido, contadorClientes, patente, marcaVehiculo, modeloVehiculo, dniDueno, contadorVehiculo, codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
								4:
									menuRepuestos(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
								5:
									menuFacturacion(orden_numero, dni_orden, patente_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_numero, factura_nro_orden, factura_dni, factura_patente, factura_fecha, factura_repuestos, factura_horas, factura_valor_hora, factura_mano_obra, factura_subtotal, factura_iva, factura_total, cantidad_facturas, nro_factura_siguiente, porcentaje_iva, dni, nombre, apellido, contadorClientes)
								6:
									menuHistorial(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_nro_orden, factura_numero, factura_total, cantidad_facturas, dni, nombre, apellido, contadorClientes)
								7:
									menuUsuarios(usuario_nombre, usuario_clave, usuario_rol, cantidad_usuarios, rol_activo)
								0:
									CerrarSesion(usuario_activo, rol_activo)
								De Otro Modo:
									Escribir "Opcion no valida."
									Pausa
							FinSegun
						Hasta Que opcion_menu = 0
					2:
						// ------------------------------------------------
						// mecanico - ordenes, repuestos e historial
						// ------------------------------------------------
						Repetir
							Limpiar Pantalla
							MostrarLogo
							Escribir "  >> USUARIO: ", usuario_activo, " | ROL: ", rol_activo
							Escribir "=================================================================="
							Escribir "  [1] Órdenes de Trabajo"
							Escribir "  [2] Repuestos y Stock"
							Escribir "  [3] Historial de Reparaciones"
							Escribir "  ----------------------------------------------------------------"
							Escribir "  [0] Cerrar Sesión"
							Escribir "=================================================================="
							Escribir Sin Saltar ">> Ingrese opción deseada: "
							Leer opcion_menu
							Segun opcion_menu Hacer
								1:
									menuOrdenes(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, nro_orden_siguiente, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, dni, nombre, apellido, contadorClientes, patente, marcaVehiculo, modeloVehiculo, dniDueno, contadorVehiculo, codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
								2:
									menuRepuestos(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
								3:
									menuHistorial(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_nro_orden, factura_numero, factura_total, cantidad_facturas, dni, nombre, apellido, contadorClientes)
								0:
									CerrarSesion(usuario_activo, rol_activo)
								De Otro Modo:
									Escribir "Opción no válida."
									Pausa
							FinSegun
						Hasta Que opcion_menu = 0
					3:
						// ------------------------------------------------
						// cliente - historial de reparaciones
						// ------------------------------------------------
						Repetir
							Limpiar Pantalla
							MostrarLogo
							Escribir "  >> USUARIO: ", usuario_activo, " | ROL: ", rol_activo
							Escribir "=================================================================="
							Escribir "  [1] Historial de Reparaciones"
							Escribir "  ----------------------------------------------------------------"
							Escribir "  [0] Cerrar Sesión"
							Escribir "=================================================================="
							Escribir Sin Saltar ">> Ingrese opción deseada: "
							Leer opcion_menu
							Segun opcion_menu Hacer
								1:
									menuHistorial(orden_numero, dni_orden, patente_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_nro_orden, factura_numero, factura_total, cantidad_facturas, dni, nombre, apellido, contadorClientes)
								0:
									CerrarSesion(usuario_activo, rol_activo)
								De Otro Modo:
									Escribir "Opción no válida."
									Pausa
							FinSegun
						Hasta Que opcion_menu = 0
				FinSegun
			FinSi
		FinSi
	Hasta Que tipoUsuario = 0
	
	Limpiar Pantalla
	MostrarLogo
	Escribir "        Muchas gracias por utilizar el Sistema Fixly!             "
	Escribir "            Sesión cerrada de manera segura.                      "
	Escribir "=================================================================="

FinAlgoritmo
