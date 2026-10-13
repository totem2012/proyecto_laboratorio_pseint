// =====================================================================
// fixly - sistema de gestion de taller mecanico
// =====================================================================

// ---------------------------------------------------------------------
// funciones generales del sistema
// ---------------------------------------------------------------------
Funcion opcionElegida <- MenuPrincipal(usuario_activo, rol_activo)
	Limpiar Pantalla
	Definir opcionElegida Como Entero
	Escribir "============================================"
	Escribir "        FIXLY - Gestion de taller"
	Escribir "  Usuario: ", usuario_activo, " | Rol: ", rol_activo
	Escribir "============================================"
	Escribir "  1 - Clientes"
	Escribir "  2 - Vehiculos"
	Escribir "  3 - Ordenes de trabajo"
	Escribir "  4 - Repuestos"
	Escribir "  5 - Facturacion"
	Escribir "  6 - Historial de reparaciones"
	Escribir "  7 - Usuarios y sesiones"
	Escribir "  0 - Salir"
	Escribir "============================================"
	Escribir Sin Saltar "Opcion: "
	Leer opcionElegida
FinFuncion

Funcion Pausa
	Definir teclaPausa Como Caracter
	Escribir Sin Saltar "Presione ENTER para continuar..."
	Leer teclaPausa
FinFuncion

Funcion dniLeido <- LeerDni(mensaje)
	Definir dniLeido Como Entero
	Repetir
		Escribir mensaje
		Leer dniLeido
		Si dniLeido <= 0 o dniLeido >= 100000000 Entonces
			Escribir "Valor de DNI invalido"
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

Funcion tieneAcceso <- VerificarPermiso(rol_actual, modulo)
	Definir tieneAcceso Como Logico
	tieneAcceso <- Falso
	Si rol_actual = "administrador" Entonces
		tieneAcceso <- Verdadero
	SiNo
		Si rol_actual = "mecanico" Entonces
			// mecanico: ordenes (3), repuestos (4), historial (6), usuarios (7)
			Si modulo = 3 o modulo = 4 o modulo = 6 o modulo = 7 Entonces
				tieneAcceso <- Verdadero
			FinSi
		SiNo
			Si rol_actual = "recepcionista" Entonces
				// recepcionista: clientes (1), vehiculos (2), facturacion (5), historial (6), usuarios (7)
				Si modulo = 1 o modulo = 2 o modulo = 5 o modulo = 6 o modulo = 7 Entonces
					tieneAcceso <- Verdadero
				FinSi
			FinSi
		FinSi
	FinSi
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo clientes
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuClientes
	Definir opcion Como Entero
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
	Leer opcion
FinFuncion

Funcion IngresarCliente(nombre, apellido, celular, dni, contadorClientes Por Referencia)
	Si contadorClientes >= 50 Entonces
		Escribir "Capacidad maxima de clientes alcanzada."
		Pausa
	SiNo
		Escribir "Ingrese los siguientes datos del cliente:"
		Escribir Sin Saltar "Nombre del cliente: "
		Leer nombre[contadorClientes]
		Escribir Sin Saltar "Apellido del cliente: "
		Leer apellido[contadorClientes]
		Escribir Sin Saltar "Nro de contacto del cliente: "
		Leer celular[contadorClientes]
		dni[contadorClientes] <- LeerDni("DNI del cliente (Sin puntos): ")
		contadorClientes <- contadorClientes + 1
		Escribir "Cliente registrado correctamente."
		Pausa
	FinSi
FinFuncion

Funcion MostrarDatosClientes(nombre, apellido, celular, dni, contadorClientes)
	Definir i Como Entero
	Si contadorClientes = 0 Entonces
		Escribir "No hay clientes registrados."
	SiNo
		Escribir "-------------------------------------------------------------"
		Escribir "Nro | NOMBRE          | APELLIDO        | CELULAR     | DNI"
		Escribir "-------------------------------------------------------------"
		Para i <- 0 Hasta contadorClientes - 1 Con Paso 1 Hacer
			Escribir i + 1, "   | ", nombre[i], " | ", apellido[i], " | ", celular[i], " | ", dni[i]
		FinPara
		Escribir "-------------------------------------------------------------"
	FinSi
FinFuncion

Funcion EliminarCliente(nombre, apellido, celular, dni, contadorClientes Por Referencia)
	Definir dniEliminar, i, j, lugarVacio Como Entero
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
		Escribir "Cliente eliminado correctamente."
	SiNo
		Escribir "Error: el DNI no se encuentra en el registro."
	FinSi
	Pausa
FinFuncion

Funcion MostrarFichaCliente(nombre, apellido, celular, dni, pos)
	Escribir "---------------------------------------------"
	Escribir "Cliente seleccionado:"
	Escribir "---------------------------------------------"
	Escribir "NOMBRE:   ", nombre[pos]
	Escribir "APELLIDO: ", apellido[pos]
	Escribir "CELULAR:  ", celular[pos]
	Escribir "DNI:      ", dni[pos]
	Escribir "---------------------------------------------"
FinFuncion

Funcion opcion <- PedirCampoCliente
	Definir opcion Como Entero
	Repetir
		Escribir "Seleccione que dato desea modificar:"
		Escribir "  1) Nombre"
		Escribir "  2) Apellido"
		Escribir "  3) Celular"
		Escribir "  4) DNI"
		Escribir Sin Saltar "Opcion: "
		Leer opcion
	Hasta Que opcion > 0 Y opcion < 5
FinFuncion

Funcion ModificarCliente(nombre, apellido, celular, dni, contadorClientes)
	Definir datoBusquedaMod, datoCampoMod Como Entero
	Si contadorClientes = 0 Entonces
		Escribir "No hay clientes registrados."
		Pausa
	SiNo
		Repetir
			Escribir "Seleccione el cliente que desea modificar:"
			MostrarDatosClientes(nombre, apellido, celular, dni, contadorClientes)
			Escribir Sin Saltar "Nro de cliente: "
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
				dni[datoBusquedaMod - 1] <- LeerDni("Ingrese el nuevo DNI: ")
		FinSegun
		Escribir "Dato actualizado correctamente."
		Pausa
	FinSi
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo vehiculos
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuVehiculos
	Definir opcion Como Entero
	Escribir "----------------------------------------"
	Escribir "        Gestion de Vehiculos"
	Escribir "----------------------------------------"
	Escribir "  1 - Ingresar un nuevo vehiculo"
	Escribir "  2 - Ver lista de vehiculos"
	Escribir "  3 - Quitar un vehiculo"
	Escribir "  4 - Modificar datos de un vehiculo"
	Escribir "  0 - Volver"
	Escribir "----------------------------------------"
	Escribir Sin Saltar "Opcion: "
	Leer opcion
FinFuncion

Funcion IngresarVehiculo(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo Por Referencia, dni, nombre, apellido, celular, contadorClientes Por Referencia)
	Definir patenteNueva, respCliente Como Caracter
	Definir dniIngresado, posCliente, posPatenteExistente Como Entero
	
	Si contadorVehiculo >= 50 Entonces
		Escribir "Capacidad maxima de vehiculos alcanzada."
		Pausa
	SiNo
		Escribir "Ingrese los siguientes datos del vehiculo:"
		Escribir Sin Saltar "Patente del vehiculo: "
		Leer patenteNueva
		
		posPatenteExistente <- BuscarPosicion(patente, contadorVehiculo, patenteNueva)
		Si posPatenteExistente <> -1 Entonces
			Escribir "Error: Ya existe un vehiculo registrado con la patente ", patenteNueva
			Pausa
		SiNo
			patente[contadorVehiculo] <- patenteNueva
			Escribir Sin Saltar "Marca del vehiculo: "
			Leer marcaVehiculo[contadorVehiculo]
			Escribir Sin Saltar "Modelo del vehiculo: "
			Leer modeloVehiculo[contadorVehiculo]
			
			dniIngresado <- LeerDni("DNI del dueno del vehiculo (Sin puntos): ")
			posCliente <- BuscarPosicion(dni, contadorClientes, dniIngresado)
			
			Si posCliente = -1 Entonces
				Escribir "Aviso: El DNI ", dniIngresado, " no corresponde a un cliente registrado."
				Escribir Sin Saltar "Desea registrar al cliente ahora mismo? (S/N): "
				Leer respCliente
				Si respCliente = "S" o respCliente = "s" Entonces
					Si contadorClientes < 50 Entonces
						Escribir Sin Saltar "Nombre del cliente: "
						Leer nombre[contadorClientes]
						Escribir Sin Saltar "Apellido del cliente: "
						Leer apellido[contadorClientes]
						Escribir Sin Saltar "Celular de contacto: "
						Leer celular[contadorClientes]
						dni[contadorClientes] <- dniIngresado
						contadorClientes <- contadorClientes + 1
						Escribir "Cliente registrado y asociado exitosamente."
					FinSi
				SiNo
					Escribir "El vehiculo quedara vinculado a dicho DNI (cliente pendiente de ficha)."
				FinSi
			SiNo
				Escribir "Dueno identificado: ", apellido[posCliente], " ", nombre[posCliente]
			FinSi
			
			dniDueno[contadorVehiculo] <- dniIngresado
			contadorVehiculo <- contadorVehiculo + 1
			Escribir "Vehiculo registrado con exito."
			Pausa
		FinSi
	FinSi
FinFuncion

Funcion MostrarDatosVehiculos(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, contadorClientes)
	Definir i, posCliente Como Entero
	Si contadorVehiculo = 0 Entonces
		Escribir "No hay vehiculos registrados."
	SiNo
		Escribir "-----------------------------------------------------------------------------------"
		Escribir "Nro | PATENTE  | MARCA        | MODELO       | DNI DUENO | DUENO"
		Escribir "-----------------------------------------------------------------------------------"
		Para i <- 0 Hasta contadorVehiculo - 1 Con Paso 1 Hacer
			posCliente <- BuscarPosicion(dni, contadorClientes, dniDueno[i])
			Si posCliente <> -1 Entonces
				Escribir i + 1, "   | ", patente[i], "  | ", marcaVehiculo[i], " | ", modeloVehiculo[i], " | ", dniDueno[i], "  | ", apellido[posCliente], " ", nombre[posCliente]
			SiNo
				Escribir i + 1, "   | ", patente[i], "  | ", marcaVehiculo[i], " | ", modeloVehiculo[i], " | ", dniDueno[i], "  | (Sin ficha)"
			FinSi
		FinPara
		Escribir "-----------------------------------------------------------------------------------"
	FinSi
FinFuncion

Funcion EliminarVehiculo(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo Por Referencia)
	Definir patenteEliminar Como Caracter
	Definir i, j, posVehiculo Como Entero
	
	Escribir Sin Saltar "Escriba la patente del vehiculo que desea eliminar: "
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
		Escribir "Vehiculo eliminado correctamente."
	SiNo
		Escribir "Error: la patente no se encuentra registrada."
	FinSi
	Pausa
FinFuncion

Funcion ModificarVehiculo(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, contadorClientes)
	Definir datoBusquedaMod, datoVehiculoMod, posCliente, nuevoDni Como Entero
	Si contadorVehiculo = 0 Entonces
		Escribir "No hay vehiculos registrados."
		Pausa
	SiNo
		Repetir
			Escribir "Seleccione el vehiculo que desea modificar:"
			MostrarDatosVehiculos(marcaVehiculo, modeloVehiculo, patente, dniDueno, contadorVehiculo, dni, nombre, apellido, contadorClientes)
			Escribir Sin Saltar "Nro de vehiculo: "
			Leer datoBusquedaMod
		Hasta Que datoBusquedaMod > 0 Y datoBusquedaMod <= contadorVehiculo
		
		Escribir "---------------------------------------------"
		Escribir "Vehiculo seleccionado: ", marcaVehiculo[datoBusquedaMod-1], " ", modeloVehiculo[datoBusquedaMod-1], " (", patente[datoBusquedaMod-1], ")"
		Escribir "---------------------------------------------"
		
		Repetir
			Escribir "Seleccione que dato desea modificar:"
			Escribir "  1) Marca"
			Escribir "  2) Modelo"
			Escribir "  3) Patente"
			Escribir "  4) DNI del dueno"
			Escribir Sin Saltar "Opcion: "
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
				nuevoDni <- LeerDni("Ingrese el nuevo DNI del dueno: ")
				dniDueno[datoBusquedaMod-1] <- nuevoDni
		FinSegun
		Escribir "Datos del vehiculo actualizados correctamente."
		Pausa
	FinSi
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo repuestos
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuRepuestos
	Limpiar Pantalla
	Definir opcion Como Entero
	Escribir "=================================="
	Escribir "       Modulo de Repuestos"
	Escribir "=================================="
	Escribir "  1 - Cargar repuesto nuevo"
	Escribir "  2 - Eliminar repuesto"
	Escribir "  3 - Buscar repuesto"
	Escribir "  4 - Modificar precio de repuesto"
	Escribir "  5 - Actualizar stock de repuesto"
	Escribir "  6 - Listado general de repuestos"
	Escribir "  0 - Volver al menu principal"
	Escribir "=================================="
	Escribir Sin Saltar "Opcion: "
	Leer opcion
FinFuncion

Funcion CargarRepuesto(codigo, nombreRepuesto, precio, stock, cantidad_repuestos Por Referencia)
	Definir codNuevo Como Caracter
	Definir posRep Como Entero
	
	Si cantidad_repuestos >= 100 Entonces
		Escribir "Capacidad maxima de repuestos alcanzada."
		Pausa
	SiNo
		Escribir Sin Saltar "Ingrese codigo del repuesto: "
		Leer codNuevo
		posRep <- BuscarPosicion(codigo, cantidad_repuestos, codNuevo)
		Si posRep <> -1 Entonces
			Escribir "Error: Ya existe un repuesto con el codigo ", codNuevo
			Pausa
		SiNo
			codigo[cantidad_repuestos] <- codNuevo
			Escribir Sin Saltar "Nombre del repuesto: "
			Leer nombreRepuesto[cantidad_repuestos]
			
			Repetir
				Escribir Sin Saltar "Precio del repuesto: $"
				Leer precio[cantidad_repuestos]
				Si precio[cantidad_repuestos] <= 0 Entonces
					Escribir "El precio debe ser mayor a cero."
				FinSi
			Hasta Que precio[cantidad_repuestos] > 0
			
			Repetir
				Escribir Sin Saltar "Stock inicial: "
				Leer stock[cantidad_repuestos]
				Si stock[cantidad_repuestos] < 0 Entonces
					Escribir "El stock no puede ser negativo."
				FinSi
			Hasta Que stock[cantidad_repuestos] >= 0
			
			cantidad_repuestos <- cantidad_repuestos + 1
			Escribir "Repuesto cargado con exito."
			Pausa
		FinSi
	FinSi
FinFuncion

Funcion EliminarRepuesto(codigo, nombreRepuesto, precio, stock, cantidad_repuestos Por Referencia)
	Definir codBuscar Como Caracter
	Definir posRep, j Como Entero
	
	Escribir Sin Saltar "Seleccionar codigo para eliminar: "
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
		Escribir "Repuesto eliminado con exito."
	SiNo
		Escribir "El codigo no existe."
	FinSi
	Pausa
FinFuncion

Funcion BuscarRepuesto(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
	Definir criterio Como Caracter
	Definir posRep Como Entero
	
	Escribir Sin Saltar "Ingrese codigo o nombre exacto del repuesto: "
	Leer criterio
	posRep <- BuscarPosicion(codigo, cantidad_repuestos, criterio)
	Si posRep = -1 Entonces
		posRep <- BuscarPosicion(nombreRepuesto, cantidad_repuestos, criterio)
	FinSi
	
	Si posRep <> -1 Entonces
		Escribir "----------------------------------"
		Escribir "Codigo: ", codigo[posRep]
		Escribir "Nombre: ", nombreRepuesto[posRep]
		Escribir "Precio: $", precio[posRep]
		Escribir "Stock:  ", stock[posRep]
		Escribir "----------------------------------"
	SiNo
		Escribir "Repuesto no encontrado."
	FinSi
	Pausa
FinFuncion

Funcion ModificarPrecioRepuesto(codigo, nombreRepuesto, precio, cantidad_repuestos)
	Definir codBuscar Como Caracter
	Definir posRep Como Entero
	Definir precioNuevo Como Real
	
	Escribir Sin Saltar "Ingrese codigo del repuesto a modificar: "
	Leer codBuscar
	posRep <- BuscarPosicion(codigo, cantidad_repuestos, codBuscar)
	
	Si posRep <> -1 Entonces
		Escribir "Repuesto: ", nombreRepuesto[posRep], " (Precio actual: $", precio[posRep], ")"
		Repetir
			Escribir Sin Saltar "Ingrese el precio nuevo: $"
			Leer precioNuevo
			Si precioNuevo <= 0 Entonces
				Escribir "El precio debe ser mayor a cero."
			FinSi
		Hasta Que precioNuevo > 0
		precio[posRep] <- precioNuevo
		Escribir "Precio actualizado exitosamente."
	SiNo
		Escribir "Repuesto no encontrado."
	FinSi
	Pausa
FinFuncion

Funcion ModificarStockRepuesto(codigo, nombreRepuesto, stock, cantidad_repuestos)
	Definir codBuscar Como Caracter
	Definir posRep, stockNuevo Como Entero
	
	Escribir Sin Saltar "Ingrese codigo del repuesto: "
	Leer codBuscar
	posRep <- BuscarPosicion(codigo, cantidad_repuestos, codBuscar)
	
	Si posRep <> -1 Entonces
		Escribir "Repuesto: ", nombreRepuesto[posRep], " (Stock actual: ", stock[posRep], ")"
		Repetir
			Escribir Sin Saltar "Ingrese el nuevo stock disponible: "
			Leer stockNuevo
			Si stockNuevo < 0 Entonces
				Escribir "El stock no puede ser negativo."
			FinSi
		Hasta Que stockNuevo >= 0
		stock[posRep] <- stockNuevo
		Escribir "Stock actualizado exitosamente."
	SiNo
		Escribir "Repuesto no encontrado."
	FinSi
	Pausa
FinFuncion

Funcion ListarRepuestos(codigo, nombreRepuesto, precio, stock, cantidad_repuestos)
	Definir i Como Entero
	Escribir "Listado de repuestos"
	Escribir "=================================================================="
	Si cantidad_repuestos = 0 Entonces
		Escribir "No hay repuestos cargados."
	SiNo
		Escribir "Nro | CODIGO   | NOMBRE                         | PRECIO    | STOCK"
		Escribir "------------------------------------------------------------------"
		Para i <- 0 Hasta cantidad_repuestos - 1 Con Paso 1 Hacer
			Escribir i + 1, "   | ", codigo[i], " | ", nombreRepuesto[i], " | $", precio[i], " | ", stock[i]
		FinPara
		Escribir "=================================================================="
	FinSi
	Pausa
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo historial de reparaciones
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuHistorial
	Limpiar Pantalla
	Definir opcion Como Entero
	Escribir "=================================="
	Escribir "     Historial de Reparaciones"
	Escribir "=================================="
	Escribir "  1 - Consultar historial por patente"
	Escribir "  2 - Consultar historial por DNI del cliente"
	Escribir "  3 - Listado general de reparaciones finalizadas"
	Escribir "  0 - Volver al menu principal"
	Escribir "=================================="
	Escribir Sin Saltar "Opcion: "
	Leer opcion
FinFuncion

Funcion ConsultarHistorialPatente(patente_orden, orden_numero, dni_orden, fecha_inicio, fecha_fin, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, detalle_nro_orden, detalle_cod_repuesto, detalle_cantidad, detalle_precio_unitario, cantidad_detalles, factura_nro_orden, factura_numero, factura_total, cantidad_facturas)
	Definir patBuscar Como Caracter
	Definir i, k, posFactura, ordenesEncontradas Como Entero
	
	Escribir Sin Saltar "Ingrese patente a consultar: "
	Leer patBuscar
	ordenesEncontradas <- 0
	
	Limpiar Pantalla
	Escribir "============================================================"
	Escribir "       HISTORIAL DE REPARACIONES - PATENTE: ", patBuscar
	Escribir "============================================================"
	
	Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
		Si patente_orden[i] = patBuscar Y estado_orden[i] = "Finalizada" Entonces
			ordenesEncontradas <- ordenesEncontradas + 1
			Escribir "------------------------------------------------------------"
			Escribir "ORDEN NRO:       ", orden_numero[i]
			Escribir "FECHA INGRESO:   ", fecha_inicio[i], " | ENTREGA: ", fecha_entrega[i]
			Escribir "DNI TITULAR:     ", dni_orden[i]
			Escribir "MOTIVO INGRESO:  ", descripcion_orden[i]
			Escribir "DIAGNOSTICO:     ", diagnostico_orden[i]
			Escribir "TRABAJO HECHO:   ", trabajo_orden[i]
			
			Escribir "Repuestos utilizados:"
			Para k <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
				Si detalle_nro_orden[k] = orden_numero[i] Entonces
					Escribir "  - ", detalle_cod_repuesto[k], " (x", detalle_cantidad[k], ") - $", detalle_precio_unitario[k] * detalle_cantidad[k]
				FinSi
			FinPara
			
			posFactura <- BuscarPosicion(factura_nro_orden, cantidad_facturas, orden_numero[i])
			Si posFactura <> -1 Entonces
				Escribir "FACTURA ASOCIADA: Factura Nro ", factura_numero[posFactura], " | Total: $", factura_total[posFactura]
			SiNo
				Escribir "FACTURA ASOCIADA: Pendiente de emision"
			FinSi
		FinSi
	FinPara
	
	Si ordenesEncontradas = 0 Entonces
		Escribir "No se registran reparaciones finalizadas para la patente ", patBuscar
	SiNo
		Escribir "------------------------------------------------------------"
		Escribir "Total de reparaciones finalizadas encontradas: ", ordenesEncontradas
	FinSi
	Escribir "============================================================"
	Pausa
FinFuncion

Funcion ConsultarHistorialDni(dni_orden, patente_orden, orden_numero, fecha_inicio, fecha_entrega, descripcion_orden, diagnostico_orden, trabajo_orden, estado_orden, cantidad_ordenes, factura_nro_orden, factura_numero, factura_total, cantidad_facturas, dni, nombre, apellido, contadorClientes)
	Definir dniBuscar, i, posCliente, posFactura, ordenesEncontradas Como Entero
	
	dniBuscar <- LeerDni("Ingrese DNI del cliente a consultar: ")
	posCliente <- BuscarPosicion(dni, contadorClientes, dniBuscar)
	ordenesEncontradas <- 0
	
	Limpiar Pantalla
	Escribir "============================================================"
	Si posCliente <> -1 Entonces
		Escribir "HISTORIAL DE REPARACIONES - CLIENTE: ", apellido[posCliente], " ", nombre[posCliente], " (DNI: ", dniBuscar, ")"
	SiNo
		Escribir "HISTORIAL DE REPARACIONES - DNI: ", dniBuscar
	FinSi
	Escribir "============================================================"
	
	Para i <- 0 Hasta cantidad_ordenes - 1 Con Paso 1 Hacer
		Si dni_orden[i] = dniBuscar Y estado_orden[i] = "Finalizada" Entonces
			ordenesEncontradas <- ordenesEncontradas + 1
			Escribir "------------------------------------------------------------"
			Escribir "ORDEN NRO:      ", orden_numero[i], " | PATENTE: ", patente_orden[i]
			Escribir "FECHA INGRESO:  ", fecha_inicio[i], " | ENTREGA: ", fecha_entrega[i]
			Escribir "FALLA/TRABAJO:  ", descripcion_orden[i], " / ", trabajo_orden[i]
			
			posFactura <- BuscarPosicion(factura_nro_orden, cantidad_facturas, orden_numero[i])
			Si posFactura <> -1 Entonces
				Escribir "FACTURA: Nro ", factura_numero[posFactura], " - Total: $", factura_total[posFactura]
			SiNo
				Escribir "FACTURA: Pendiente"
			FinSi
		FinSi
	FinPara
	
	Si ordenesEncontradas = 0 Entonces
		Escribir "No se registran reparaciones finalizadas para el DNI ", dniBuscar
	SiNo
		Escribir "------------------------------------------------------------"
		Escribir "Total de reparaciones del cliente: ", ordenesEncontradas
	FinSi
	Escribir "============================================================"
	Pausa
FinFuncion

Funcion ListarHistorialGeneral(orden_numero, patente_orden, dni_orden, fecha_inicio, fecha_entrega, trabajo_orden, estado_orden, cantidad_ordenes, factura_nro_orden, factura_total, cantidad_facturas)
	Definir i, posFactura, totalFinalizadas Como Entero
	Limpiar Pantalla
	Escribir "==================================================================================="
	Escribir "                   LISTADO GENERAL DE REPARACIONES FINALIZADAS"
	Escribir "==================================================================================="
	totalFinalizadas <- 0
	Escribir "ORDEN | PATENTE  | DNI DUENO | INGRESO    | ENTREGA    | TOTAL FACTURADO"
	Escribir "-----------------------------------------------------------------------------------"
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
		Escribir "No hay reparaciones finalizadas registradas."
	SiNo
		Escribir "-----------------------------------------------------------------------------------"
		Escribir "Total de reparaciones finalizadas en el taller: ", totalFinalizadas
	FinSi
	Escribir "==================================================================================="
	Pausa
FinFuncion

// ---------------------------------------------------------------------
// funciones del modulo usuarios y sesiones
// ---------------------------------------------------------------------
Funcion opcion <- MostrarMenuUsuarios
	Limpiar Pantalla
	Definir opcion Como Entero
	Escribir "=================================="
	Escribir "     Gestion de Usuarios y Rol"
	Escribir "=================================="
	Escribir "  1 - Ver usuarios registrados"
	Escribir "  2 - Registrar nuevo usuario"
	Escribir "  3 - Cambiar usuario / Iniciar sesion"
	Escribir "  0 - Volver al menu principal"
	Escribir "=================================="
	Escribir Sin Saltar "Opcion: "
	Leer opcion
FinFuncion

Funcion ListarUsuarios(usuario_nombre, usuario_rol, cantidad_usuarios)
	Definir i Como Entero
	Escribir "Usuarios registrados en el sistema:"
	Escribir "------------------------------------------------"
	Escribir "Nro | NOMBRE DE USUARIO | ROL ASIGNADO"
	Escribir "------------------------------------------------"
	Para i <- 0 Hasta cantidad_usuarios - 1 Con Paso 1 Hacer
		Escribir i + 1, "   | ", usuario_nombre[i], "         | ", usuario_rol[i]
	FinPara
	Escribir "------------------------------------------------"
	Pausa
FinFuncion

Funcion RegistrarUsuario(usuario_nombre, usuario_clave, usuario_rol, cantidad_usuarios Por Referencia, rol_activo)
	Definir nuevoUser, nuevaPass Como Caracter
	Definir opcRol, posUser Como Entero
	
	Si rol_activo <> "administrador" Entonces
		Escribir "Acceso denegado: solo un administrador puede registrar nuevos usuarios."
		Pausa
	SiNo
		Si cantidad_usuarios >= 20 Entonces
			Escribir "Limite de usuarios alcanzado."
			Pausa
		SiNo
			Escribir Sin Saltar "Nombre de usuario nuevo: "
			Leer nuevoUser
			posUser <- BuscarPosicion(usuario_nombre, cantidad_usuarios, nuevoUser)
			Si posUser <> -1 Entonces
				Escribir "Error: el nombre de usuario ya existe."
				Pausa
			SiNo
				usuario_nombre[cantidad_usuarios] <- nuevoUser
				Escribir Sin Saltar "Contrasena de acceso: "
				Leer usuario_clave[cantidad_usuarios]
				
				Repetir
					Escribir "Seleccione rol del usuario:"
					Escribir "  1 - Administrador (Acceso total)"
					Escribir "  2 - Mecanico (Ordenes, Repuestos, Historial)"
					Escribir "  3 - Recepcionista (Clientes, Vehiculos, Facturas, Historial)"
					Escribir Sin Saltar "Opcion: "
					Leer opcRol
				Hasta Que opcRol >= 1 Y opcRol <= 3
				
				Segun opcRol Hacer
					1:
						usuario_rol[cantidad_usuarios] <- "administrador"
					2:
						usuario_rol[cantidad_usuarios] <- "mecanico"
					3:
						usuario_rol[cantidad_usuarios] <- "recepcionista"
				FinSegun
				
				cantidad_usuarios <- cantidad_usuarios + 1
				Escribir "Usuario registrado exitosamente."
				Pausa
			FinSi
		FinSi
	FinSi
FinFuncion

Funcion IniciarSesion(usuario_nombre, usuario_clave, usuario_rol, cantidad_usuarios, usuario_activo Por Referencia, rol_activo Por Referencia)
	Definir userIngresado, passIngresada Como Caracter
	Definir posUser Como Entero
	
	Escribir Sin Saltar "Usuario: "
	Leer userIngresado
	posUser <- BuscarPosicion(usuario_nombre, cantidad_usuarios, userIngresado)
	
	Si posUser = -1 Entonces
		Escribir "Error: usuario no encontrado."
		Pausa
	SiNo
		Escribir Sin Saltar "Contrasena: "
		Leer passIngresada
		Si passIngresada = usuario_clave[posUser] Entonces
			usuario_activo <- usuario_nombre[posUser]
			rol_activo <- usuario_rol[posUser]
			Escribir "Sesion iniciada con exito. Bienvenido/a ", usuario_activo, " (Rol: ", rol_activo, ")."
		SiNo
			Escribir "Error: contrasena incorrecta."
		FinSi
		Pausa
	FinSi
FinFuncion


// =====================================================================
// algoritmo principal
// =====================================================================
Algoritmo Fixly
	
	Definir opcion, op Como Entero
	Definir tecla Como Caracter
	Definir i, j Como Entero
	
	// variables de sesion y usuarios
	Definir cantidad_usuarios, opcion_usuarios Como Entero
	Definir usuario_nombre, usuario_clave, usuario_rol Como Caracter
	Definir usuario_activo, rol_activo Como Caracter
	Dimension usuario_nombre[20]
	Dimension usuario_clave[20]
	Dimension usuario_rol[20]
	cantidad_usuarios <- 0
	
	// variables de opcion vehiculo
	Definir posicionEliminar, menuVehiculo, contadorVehiculo, dniDueno Como Entero
	Definir marcaVehiculo, modeloVehiculo, patente Como Caracter
	Dimensionar marcaVehiculo[50], modeloVehiculo[50], patente[50], dniDueno[50]
	Definir vehiculoEliminado, lugarVacio, datoBusquedaMod, datoVehiculoMod Como Entero
	Definir patenteEliminar Como Caracter
	contadorVehiculo <- 0
	posicionEliminar <- 0
	
	// variables de opcion cliente
	Definir contadorClientes, menuClientes, dniEliminar, clienteEliminado Como Entero
	Definir nombre, apellido, celular Como Caracter
	Definir dni Como Entero
	Dimensionar nombre[50], apellido[50], celular[50], dni[50]
	contadorClientes <- 0
	
	// variables de opcion repuestos
	Definir cantidad_repuestos, opcion_repuestos, posicion_encontrada, stock Como Entero
	Definir codigo, nombreRepuesto, codigo_buscar, nombre_buscar Como Caracter
	Definir precio, precio_nuevo Como Real
	Dimension codigo[100]
	Dimension nombreRepuesto[100]
	Dimension precio[100]
	Dimension stock[100]
	cantidad_repuestos <- 0
	
	// variables de opcion ordenes
	Definir opcion_ordenes, dni_buscar, posicion_cliente, posicion_vehiculo, posicion_repuesto, posicion_orden, orden_numero, orden_buscar Como Entero
	Definir cantidad_ordenes, cantidad_detalles, nro_orden_siguiente, cantidad_repuestos_ot, k, dni_orden, opcion_modificar_ot Como Entero
	Definir posicion_repuesto_modificar, detalle_cantidad, detalle_nro_orden, posicion_detalle Como Entero
	Definir detalle_precio_unitario Como Real
	Definir tiene_repuestos Como Entero
	Definir patente_buscar, confirma_repuesto, falla_detalle, nombre_repuesto_buscar, fecha_inicio, fecha_fin, fecha_entrega, patente_orden, estado_orden Como Caracter
	Definir descripcion_orden, diagnostico_orden, nombre_repuesto_modificar, detalle_cod_repuesto, codigo_quitar Como Caracter
	Definir trabajo_orden Como Caracter
	Definir encontrado_detalle Como Logico
	cantidad_ordenes <- 0
	cantidad_detalles <- 0
	nro_orden_siguiente <- 1
	
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
	
	// variables de facturacion
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
	
	// variables de historial
	Definir opcion_historial Como Entero
	
	// --- carga de datos de prueba integrados ---
	
	// 1. usuarios
	cantidad_usuarios <- 3
	usuario_nombre[0] <- "admin"
	usuario_clave[0] <- "admin123"
	usuario_rol[0] <- "administrador"
	
	usuario_nombre[1] <- "mecanico"
	usuario_clave[1] <- "meca123"
	usuario_rol[1] <- "mecanico"
	
	usuario_nombre[2] <- "recepcion"
	usuario_clave[2] <- "recep123"
	usuario_rol[2] <- "recepcionista"
	
	usuario_activo <- "admin"
	rol_activo <- "administrador"
	
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
	
	
	Repetir
		op <- MenuPrincipal(usuario_activo, rol_activo)
		
		Si op <> 0 Y VerificarPermiso(rol_activo, op) = Falso Entonces
			Escribir "Acceso denegado para el rol actual."
			Pausa
		SiNo
			Segun op Hacer
				1:
					// ------------------------------------------------
					// modulo clientes - inicio
					// ------------------------------------------------
					menuClientes <- MostrarMenuClientes
					Segun menuClientes Hacer
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
							Escribir "Opcion no valida."
							Pausa
					FinSegun
					// ------------------------------------------------
					// modulo clientes - fin
					// ------------------------------------------------
					
				2:
					// ------------------------------------------------
					// modulo vehiculos - inicio
					// ------------------------------------------------
					menuVehiculo <- MostrarMenuVehiculos
					Segun menuVehiculo Hacer
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
							Escribir "Opcion no valida."
							Pausa
					FinSegun
					// ------------------------------------------------
					// modulo vehiculos - fin
					// ------------------------------------------------
					
				3:
					// ------------------------------------------------
					// modulo ordenes de trabajo - inicio
					// ------------------------------------------------
					Repetir
						Limpiar Pantalla
						Escribir "=================================="
						Escribir "     Modulo Ordenes de Trabajo"
						Escribir "=================================="
						Escribir "  1 - Cargar orden de trabajo"
						Escribir "  2 - Modificar orden de trabajo"
						Escribir "  3 - Buscar orden por patente"
						Escribir "  4 - Cerrar orden de trabajo"
						Escribir "  0 - Volver al menu principal"
						Escribir "=================================="
						Escribir Sin Saltar "Opcion: "
						Leer opcion_ordenes
						
						Segun opcion_ordenes Hacer
							1:
								Escribir "Ingrese los datos de la nueva orden:"
								Escribir Sin Saltar "Patente del vehiculo: "
								Leer patente_buscar
								posicion_vehiculo <- BuscarPosicion(patente, contadorVehiculo, patente_buscar)
								
								Si posicion_vehiculo = -1 Entonces
									Escribir "Error: el vehiculo con patente ", patente_buscar, " no esta registrado en el sistema."
									Escribir "Cargue primero el vehiculo en el Modulo 2."
									Pausa
								SiNo
									dni_orden[cantidad_ordenes] <- dniDueno[posicion_vehiculo]
									patente_orden[cantidad_ordenes] <- patente_buscar
									
									posicion_cliente <- BuscarPosicion(dni, contadorClientes, dniDueno[posicion_vehiculo])
									Si posicion_cliente <> -1 Entonces
										Escribir "Vehiculo: ", marcaVehiculo[posicion_vehiculo], " ", modeloVehiculo[posicion_vehiculo], " | Titular: ", apellido[posicion_cliente], " ", nombre[posicion_cliente]
									SiNo
										Escribir "Vehiculo: ", marcaVehiculo[posicion_vehiculo], " ", modeloVehiculo[posicion_vehiculo], " | DNI Titular: ", dniDueno[posicion_vehiculo]
									FinSi
									
									Escribir Sin Saltar "Fecha de inicio (DD/MM/AAAA): "
									Leer fecha_inicio[cantidad_ordenes]
									fecha_fin[cantidad_ordenes] <- ""
									fecha_entrega[cantidad_ordenes] <- ""
									
									Escribir Sin Saltar "Motivo de ingreso / descripcion del problema: "
									Leer descripcion_orden[cantidad_ordenes]
									
									Escribir Sin Saltar "Diagnostico inicial: "
									Leer diagnostico_orden[cantidad_ordenes]
									trabajo_orden[cantidad_ordenes] <- ""
									
									orden_numero[cantidad_ordenes] <- nro_orden_siguiente
									nro_orden_siguiente <- nro_orden_siguiente + 1
									estado_orden[cantidad_ordenes] <- "Pendiente"
									
									Escribir Sin Saltar "Desea agregar repuestos a la orden ahora? (1: Si / 0: No): "
									Leer tiene_repuestos
									Si tiene_repuestos = 1 Entonces
										Repetir
											Escribir Sin Saltar "Codigo de repuesto a agregar (o FIN para terminar): "
											Leer nombre_repuesto_buscar
											
											Si nombre_repuesto_buscar <> "FIN" Y nombre_repuesto_buscar <> "fin" Entonces
												posicion_repuesto <- BuscarPosicion(codigo, cantidad_repuestos, nombre_repuesto_buscar)
												Si posicion_repuesto = -1 Entonces
													Escribir "Repuesto no encontrado en el catalogo."
												SiNo
													Escribir "Repuesto: ", nombreRepuesto[posicion_repuesto], " | Stock disponible: ", stock[posicion_repuesto], " | Precio: $", precio[posicion_repuesto]
													Repetir
														Escribir Sin Saltar "Cantidad requerida: "
														Leer cantidad_repuestos_ot
														Si cantidad_repuestos_ot <= 0 Entonces
															Escribir "La cantidad debe ser mayor a cero."
														FinSi
													Hasta Que cantidad_repuestos_ot > 0
													
													Si cantidad_repuestos_ot > stock[posicion_repuesto] Entonces
														Escribir "Stock insuficiente. No se puede agregar esa cantidad."
													SiNo
														detalle_nro_orden[cantidad_detalles] <- orden_numero[cantidad_ordenes]
														detalle_cod_repuesto[cantidad_detalles] <- codigo[posicion_repuesto]
														detalle_cantidad[cantidad_detalles] <- cantidad_repuestos_ot
														detalle_precio_unitario[cantidad_detalles] <- precio[posicion_repuesto]
														cantidad_detalles <- cantidad_detalles + 1
														
														// descontar stock real del catalogo
														stock[posicion_repuesto] <- stock[posicion_repuesto] - cantidad_repuestos_ot
														Escribir "Repuesto agregado a la orden y descontado del inventario."
													FinSi
												FinSi
											FinSi
										Hasta Que nombre_repuesto_buscar = "FIN" o nombre_repuesto_buscar = "fin"
									FinSi
									
									cantidad_ordenes <- cantidad_ordenes + 1
									Escribir "Orden Nro ", orden_numero[cantidad_ordenes-1], " registrada con exito."
									Pausa
								FinSi
								
							2:
								Escribir Sin Saltar "Ingrese patente de la orden a modificar: "
								Leer patente_buscar
								posicion_orden <- BuscarPosicion(patente_orden, cantidad_ordenes, patente_buscar)
								
								Si posicion_orden = -1 Entonces
									Escribir "No hay orden registrada para la patente ", patente_buscar
									Pausa
								SiNo
									Si estado_orden[posicion_orden] = "Finalizada" Entonces
										Escribir "La orden Nro ", orden_numero[posicion_orden], " ya esta Finalizada y no puede modificarse."
										Pausa
									SiNo
										Repetir
											Limpiar Pantalla
											Escribir "=================================="
											Escribir "  Modificar Orden Nro ", orden_numero[posicion_orden]
											Escribir "=================================="
											Escribir "  1 - Modificar diagnostico"
											Escribir "  2 - Agregar repuestos a la orden"
											Escribir "  3 - Quitar repuesto de la orden"
											Escribir "  0 - Volver al menu de ordenes"
											Escribir "=================================="
											Escribir Sin Saltar "Opcion: "
											Leer opcion_modificar_ot
											
											Segun opcion_modificar_ot Hacer
												1:
													Escribir "Diagnostico actual: ", diagnostico_orden[posicion_orden]
													Escribir Sin Saltar "Nuevo diagnostico: "
													Leer diagnostico_orden[posicion_orden]
													Escribir "Diagnostico actualizado."
													Pausa
												2:
													Escribir Sin Saltar "Codigo de repuesto a agregar: "
													Leer nombre_repuesto_buscar
													posicion_repuesto <- BuscarPosicion(codigo, cantidad_repuestos, nombre_repuesto_buscar)
													
													Si posicion_repuesto = -1 Entonces
														Escribir "Repuesto no encontrado en el catalogo."
													SiNo
														Escribir "Stock actual: ", stock[posicion_repuesto]
														Escribir Sin Saltar "Cantidad requerida: "
														Leer cantidad_repuestos_ot
														Si cantidad_repuestos_ot > stock[posicion_repuesto] Entonces
															Escribir "Stock insuficiente."
														SiNo
															detalle_nro_orden[cantidad_detalles] <- orden_numero[posicion_orden]
															detalle_cod_repuesto[cantidad_detalles] <- codigo[posicion_repuesto]
															detalle_cantidad[cantidad_detalles] <- cantidad_repuestos_ot
															detalle_precio_unitario[cantidad_detalles] <- precio[posicion_repuesto]
															cantidad_detalles <- cantidad_detalles + 1
															stock[posicion_repuesto] <- stock[posicion_repuesto] - cantidad_repuestos_ot
															Escribir "Repuesto agregado y descontado del stock."
														FinSi
													FinSi
													Pausa
												3:
													Escribir "Repuestos cargados en la orden:"
													Para k <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
														Si detalle_nro_orden[k] = orden_numero[posicion_orden] Entonces
															Escribir "  - Codigo: ", detalle_cod_repuesto[k], " (x", detalle_cantidad[k], ")"
														FinSi
													FinPara
													Escribir Sin Saltar "Codigo de repuesto a quitar: "
													Leer codigo_quitar
													
													posicion_detalle <- -1
													Para k <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
														Si detalle_nro_orden[k] = orden_numero[posicion_orden] Y detalle_cod_repuesto[k] = codigo_quitar Entonces
															posicion_detalle <- k
														FinSi
													FinPara
													
													Si posicion_detalle = -1 Entonces
														Escribir "El repuesto no esta asignado a esta orden."
													SiNo
														// devolver stock al catalogo
														posicion_repuesto <- BuscarPosicion(codigo, cantidad_repuestos, codigo_quitar)
														Si posicion_repuesto <> -1 Entonces
															stock[posicion_repuesto] <- stock[posicion_repuesto] + detalle_cantidad[posicion_detalle]
														FinSi
														
														Para k <- posicion_detalle Hasta cantidad_detalles - 2 Con Paso 1 Hacer
															detalle_nro_orden[k] <- detalle_nro_orden[k+1]
															detalle_cod_repuesto[k] <- detalle_cod_repuesto[k+1]
															detalle_cantidad[k] <- detalle_cantidad[k+1]
															detalle_precio_unitario[k] <- detalle_precio_unitario[k+1]
														FinPara
														cantidad_detalles <- cantidad_detalles - 1
														Escribir "Repuesto quitado y restaurado al stock general."
													FinSi
													Pausa
												0:
													Escribir "Volviendo..."
											FinSegun
										Mientras Que opcion_modificar_ot <> 0
									FinSi
								FinSi
								
							3:
								Escribir Sin Saltar "Patente del vehiculo a buscar: "
								Leer patente_buscar
								posicion_orden <- BuscarPosicion(patente_orden, cantidad_ordenes, patente_buscar)
								
								Si posicion_orden = -1 Entonces
									Escribir "No hay orden registrada para la patente ", patente_buscar
									Pausa
								SiNo
									Limpiar Pantalla
									Escribir "=================================="
									Escribir "  ORDEN Nro ", orden_numero[posicion_orden]
									Escribir "=================================="
									Escribir "Patente:         ", patente_orden[posicion_orden]
									Escribir "DNI titular:     ", dni_orden[posicion_orden]
									Escribir "Estado:          ", estado_orden[posicion_orden]
									Escribir "Fecha ingreso:   ", fecha_inicio[posicion_orden]
									Si estado_orden[posicion_orden] = "Finalizada" Entonces
										Escribir "Fecha entrega:   ", fecha_entrega[posicion_orden]
									FinSi
									Escribir "Motivo ingreso:  ", descripcion_orden[posicion_orden]
									Escribir "Diagnostico:     ", diagnostico_orden[posicion_orden]
									Si trabajo_orden[posicion_orden] <> "" Entonces
										Escribir "Trabajo hecho:   ", trabajo_orden[posicion_orden]
									FinSi
									Escribir "Repuestos utilizados:"
									Para k <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
										Si detalle_nro_orden[k] = orden_numero[posicion_orden] Entonces
											Escribir "  - ", detalle_cod_repuesto[k], " (x", detalle_cantidad[k], ") - $", detalle_precio_unitario[k] * detalle_cantidad[k]
										FinSi
									FinPara
									Escribir "=================================="
									Pausa
								FinSi
								
							4:
								Escribir Sin Saltar "Patente del vehiculo a cerrar: "
								Leer patente_buscar
								posicion_orden <- BuscarPosicion(patente_orden, cantidad_ordenes, patente_buscar)
								
								Si posicion_orden = -1 Entonces
									Escribir "No hay orden registrada para la patente ", patente_buscar
									Pausa
								SiNo
									Si estado_orden[posicion_orden] = "Finalizada" Entonces
										Escribir "La orden Nro ", orden_numero[posicion_orden], " ya se encuentra Finalizada."
										Pausa
									SiNo
										Limpiar Pantalla
										Escribir "=================================="
										Escribir "  Cierre de Orden Nro ", orden_numero[posicion_orden]
										Escribir "=================================="
										Escribir Sin Saltar "Fecha de finalizacion (DD/MM/AAAA): "
										Leer fecha_fin[posicion_orden]
										
										Escribir Sin Saltar "Detalle de trabajo realizado por el mecanico: "
										Leer trabajo_orden[posicion_orden]
										
										Escribir Sin Saltar "Confirmar cierre definitivo de la orden? (S/N): "
										Leer confirma_repuesto
										
										Si confirma_repuesto = "S" o confirma_repuesto = "s" Entonces
											estado_orden[posicion_orden] <- "Finalizada"
											fecha_entrega[posicion_orden] <- fecha_fin[posicion_orden]
											Escribir "=================================="
											Escribir "Orden cerrada exitosamente y lista para facturacion."
											Escribir "=================================="
										SiNo
											Escribir "Cierre cancelado."
										FinSi
										Pausa
									FinSi
								FinSi
								
							0:
								Escribir "Volviendo al menu principal..."
							De Otro Modo:
								Escribir "Opcion no valida."
								Pausa
						FinSegun
					Mientras Que opcion_ordenes <> 0
					// ------------------------------------------------
					// modulo ordenes de trabajo - fin
					// ------------------------------------------------
					
				4:
					// ------------------------------------------------
					// modulo repuestos - inicio
					// ------------------------------------------------
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
								Escribir "Opcion no valida."
								Pausa
						FinSegun
					Mientras Que opcion_repuestos <> 0
					// ------------------------------------------------
					// modulo repuestos - fin
					// ------------------------------------------------
					
				5:
					// ------------------------------------------------
					// modulo facturacion - inicio
					// ------------------------------------------------
					Repetir
						Limpiar Pantalla
						Escribir "=================================="
						Escribir "     Modulo de Facturacion"
						Escribir "=================================="
						Escribir "  1 - Emitir factura de una orden"
						Escribir "  2 - Ver una factura"
						Escribir "  3 - Listado de facturas emitidas"
						Escribir "  4 - Facturas de un cliente"
						Escribir "  0 - Volver al menu principal"
						Escribir "=================================="
						Escribir Sin Saltar "Opcion: "
						Leer opcion_facturacion
						
						Segun opcion_facturacion Hacer
							1:
								Limpiar Pantalla
								Escribir "=================================="
								Escribir "        Emitir Factura"
								Escribir "=================================="
								Si cantidad_facturas >= 100 Entonces
									Escribir "Cantidad maxima de facturas alcanzada."
									Pausa
								SiNo
									Escribir Sin Saltar "Numero de orden a facturar: "
									Leer orden_facturar
									posicion_orden_factura <- BuscarPosicion(orden_numero, cantidad_ordenes, orden_facturar)
									
									Si BuscarPosicion(factura_nro_orden, cantidad_facturas, orden_facturar) <> -1 Entonces
										orden_ya_facturada <- Verdadero
									SiNo
										orden_ya_facturada <- Falso
									FinSi
									
									Si posicion_orden_factura = -1 Entonces
										Escribir "La orden no existe."
										Pausa
									SiNo
										Si estado_orden[posicion_orden_factura] <> "Finalizada" Entonces
											Escribir "La orden todavia esta pendiente."
											Escribir "Debe finalizarse en el Modulo 3 antes de emitir la factura."
											Pausa
										SiNo
											Si orden_ya_facturada Entonces
												Escribir "La orden Nro ", orden_facturar, " ya fue facturada anteriormente."
												Pausa
											SiNo
												posicion_cliente_factura <- BuscarPosicion(dni, contadorClientes, dni_orden[posicion_orden_factura])
												Escribir "Orden Nro:  ", orden_numero[posicion_orden_factura]
												Si posicion_cliente_factura <> -1 Entonces
													Escribir "Cliente:    ", apellido[posicion_cliente_factura], " ", nombre[posicion_cliente_factura]
												FinSi
												Escribir "DNI:        ", dni_orden[posicion_orden_factura]
												Escribir "Patente:    ", patente_orden[posicion_orden_factura]
												Escribir "Trabajo:    ", trabajo_orden[posicion_orden_factura]
												Escribir "----------------------------------"
												Escribir "Repuestos utilizados:"
												total_repuestos_calc <- 0
												Para k <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
													Si detalle_nro_orden[k] = orden_numero[posicion_orden_factura] Entonces
														Escribir "  - ", detalle_cod_repuesto[k], " x ", detalle_cantidad[k], " = $", detalle_precio_unitario[k] * detalle_cantidad[k]
														total_repuestos_calc <- total_repuestos_calc + detalle_precio_unitario[k] * detalle_cantidad[k]
													FinSi
												FinPara
												Si total_repuestos_calc = 0 Entonces
													Escribir "  (sin repuestos utilizados)"
												FinSi
												Escribir "----------------------------------"
												
												Repetir
													Escribir Sin Saltar "Horas de mano de obra trabajadas: "
													Leer horas_ingresadas
													Si horas_ingresadas <= 0 Entonces
														Escribir "Las horas deben ser mayores a cero."
													FinSi
												Hasta Que horas_ingresadas > 0
												
												Repetir
													Escribir Sin Saltar "Valor por hora de mano de obra: $"
													Leer valor_hora_ingresado
													Si valor_hora_ingresado <= 0 Entonces
														Escribir "El valor debe ser mayor a cero."
													FinSi
												Hasta Que valor_hora_ingresado > 0
												
												Escribir Sin Saltar "Fecha de emision (DD/MM/AAAA): "
												Leer fecha_factura_ingresada
												
												mano_obra_calc <- horas_ingresadas * valor_hora_ingresado
												subtotal_calc <- total_repuestos_calc + mano_obra_calc
												iva_calc <- redon(subtotal_calc * porcentaje_iva) / 100
												total_calc <- subtotal_calc + iva_calc
												
												Escribir "=================================="
												Escribir "Repuestos:        $", total_repuestos_calc
												Escribir "Mano de obra:     $", mano_obra_calc, " (", horas_ingresadas, " hs x $", valor_hora_ingresado, ")"
												Escribir "Subtotal:         $", subtotal_calc
												Escribir "IVA (", porcentaje_iva, "%):        $", iva_calc
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
													Escribir "Factura Nro ", factura_numero[cantidad_facturas - 1], " emitida correctamente."
												SiNo
													Escribir "Emision cancelada."
												FinSi
												Pausa
											FinSi
										FinSi
									FinSi
								FinSi
								
							2:
								Limpiar Pantalla
								Escribir "=================================="
								Escribir "          Ver Factura"
								Escribir "=================================="
								Escribir Sin Saltar "Numero de factura: "
								Leer factura_buscar
								posicion_factura <- BuscarPosicion(factura_numero, cantidad_facturas, factura_buscar)
								
								Si posicion_factura = -1 Entonces
									Escribir "La factura Nro ", factura_buscar, " no existe."
								SiNo
									posicion_cliente_factura <- BuscarPosicion(dni, contadorClientes, factura_dni[posicion_factura])
									Limpiar Pantalla
									Escribir "=================================="
									Escribir "  FIXLY - Taller Mecanico"
									Escribir "  FACTURA Nro ", factura_numero[posicion_factura]
									Escribir "=================================="
									Escribir "Fecha:      ", factura_fecha[posicion_factura]
									Escribir "Orden Nro:  ", factura_nro_orden[posicion_factura]
									Si posicion_cliente_factura <> -1 Entonces
										Escribir "Cliente:    ", apellido[posicion_cliente_factura], " ", nombre[posicion_cliente_factura]
									FinSi
									Escribir "DNI:        ", factura_dni[posicion_factura]
									Escribir "Patente:    ", factura_patente[posicion_factura]
									Escribir "----------------------------------"
									Escribir "Repuestos detallados:"
									Para k <- 0 Hasta cantidad_detalles - 1 Con Paso 1 Hacer
										Si detalle_nro_orden[k] = factura_nro_orden[posicion_factura] Entonces
											Escribir "  - ", detalle_cod_repuesto[k], " x ", detalle_cantidad[k], " = $", detalle_precio_unitario[k] * detalle_cantidad[k]
										FinSi
									FinPara
									Escribir "----------------------------------"
									Escribir "Subtotal Repuestos:   $", factura_repuestos[posicion_factura]
									Escribir "Mano de obra:         $", factura_mano_obra[posicion_factura], " (", factura_horas[posicion_factura], " hs x $", factura_valor_hora[posicion_factura], ")"
									Escribir "Subtotal Gravado:     $", factura_subtotal[posicion_factura]
									Escribir "IVA (", porcentaje_iva, "%):            $", factura_iva[posicion_factura]
									Escribir "TOTAL A PAGAR:        $", factura_total[posicion_factura]
									Escribir "=================================="
								FinSi
								Pausa
								
							3:
								Limpiar Pantalla
								Escribir "=================================="
								Escribir "      Facturas Emitidas"
								Escribir "=================================="
								Si cantidad_facturas = 0 Entonces
									Escribir "No hay facturas emitidas."
								SiNo
									total_facturado <- 0
									Escribir "NRO | ORDEN | FECHA      | PATENTE  | TOTAL"
									Escribir "---------------------------------------------"
									Para i <- 0 Hasta cantidad_facturas - 1 Con Paso 1 Hacer
										Escribir factura_numero[i], "   | ", factura_nro_orden[i], "     | ", factura_fecha[i], " | ", factura_patente[i], "  | $", factura_total[i]
										total_facturado <- total_facturado + factura_total[i]
									FinPara
									Escribir "---------------------------------------------"
									Escribir "Facturas emitidas: ", cantidad_facturas
									Escribir "Total facturado:   $", total_facturado
								FinSi
								Pausa
								
							4:
								Limpiar Pantalla
								Escribir "=================================="
								Escribir "     Facturas de un Cliente"
								Escribir "=================================="
								dni_factura_buscar <- LeerDni("DNI del cliente: ")
								facturas_encontradas <- 0
								total_facturado <- 0
								
								Para i <- 0 Hasta cantidad_facturas - 1 Con Paso 1 Hacer
									Si factura_dni[i] = dni_factura_buscar Entonces
										Si facturas_encontradas = 0 Entonces
											Escribir "NRO | ORDEN | FECHA      | PATENTE  | TOTAL"
											Escribir "---------------------------------------------"
										FinSi
										Escribir factura_numero[i], "   | ", factura_nro_orden[i], "     | ", factura_fecha[i], " | ", factura_patente[i], "  | $", factura_total[i]
										facturas_encontradas <- facturas_encontradas + 1
										total_facturado <- total_facturado + factura_total[i]
									FinSi
								FinPara
								
								Si facturas_encontradas = 0 Entonces
									Escribir "No hay facturas para el DNI ", dni_factura_buscar
								SiNo
									Escribir "---------------------------------------------"
									Escribir "Facturas del cliente: ", facturas_encontradas
									Escribir "Total facturado:      $", total_facturado
								FinSi
								Pausa
								
							0:
								Escribir "Volviendo al menu principal..."
							De Otro Modo:
								Escribir "Opcion no valida."
								Pausa
						FinSegun
					Mientras Que opcion_facturacion <> 0
					// ------------------------------------------------
					// modulo facturacion - fin
					// ------------------------------------------------
					
				6:
					// ------------------------------------------------
					// modulo historial de reparaciones - inicio
					// ------------------------------------------------
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
								Escribir "Opcion no valida."
								Pausa
						FinSegun
					Mientras Que opcion_historial <> 0
					// ------------------------------------------------
					// modulo historial de reparaciones - fin
					// ------------------------------------------------
					
				7:
					// ------------------------------------------------
					// modulo usuarios y sesiones - inicio
					// ------------------------------------------------
					Repetir
						opcion_usuarios <- MostrarMenuUsuarios
						Segun opcion_usuarios Hacer
							1:
								ListarUsuarios(usuario_nombre, usuario_rol, cantidad_usuarios)
							2:
								RegistrarUsuario(usuario_nombre, usuario_clave, usuario_rol, cantidad_usuarios, rol_activo)
							3:
								IniciarSesion(usuario_nombre, usuario_clave, usuario_rol, cantidad_usuarios, usuario_activo, rol_activo)
							0:
								Escribir "Volviendo al menu principal..."
							De Otro Modo:
								Escribir "Opcion no valida."
								Pausa
						FinSegun
					Mientras Que opcion_usuarios <> 0
					// ------------------------------------------------
					// modulo usuarios y sesiones - fin
					// ------------------------------------------------
					
				0:
					Escribir "Cerrando el sistema Fixly. Hasta luego."
					
				De Otro Modo:
					Escribir "Opcion no valida."
					Pausa
			FinSegun
		FinSi
		
	Hasta Que op = 0

FinAlgoritmo
