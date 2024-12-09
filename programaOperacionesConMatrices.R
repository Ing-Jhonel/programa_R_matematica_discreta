# Funciones para operaciones con matrices
sumar_matrices <- function(A, B) {
  if (!all(dim(A) == dim(B))) {
    stop("Las matrices deben tener las mismas dimensiones para sumarlas.")
  }
  return(A + B)
}

restar_matrices <- function(A, B) {
  if (!all(dim(A) == dim(B))) {
    stop("Las matrices deben tener las mismas dimensiones para restarlas.")
  }
  return(A - B)
}

multiplicar_matrices <- function(A, B) {
  if (ncol(A) != nrow(B)) {
    stop("El número de columnas de la primera matriz debe ser igual al número de filas de la segunda matriz.")
  }
  return(A %*% B)
}

inversa_matriz <- function(A) {
  if (nrow(A) != ncol(A)) {
    stop("La matriz debe ser cuadrada para calcular su inversa.")
  }
  if (det(A) == 0) {
    stop("La matriz es singular y no tiene inversa.")
  }
  return(solve(A))
}

# Menú interactivo para realizar operaciones
menu <- function() {
  cat("\n*** Operaciones con Matrices ***\n")
  cat("1. Sumar matrices\n")
  cat("2. Restar matrices\n")
  cat("3. Multiplicar matrices\n")
  cat("4. Hallar inversa de una matriz\n")
  cat("5. Salir\n")
  
  opcion <- as.integer(readline("Seleccione una opción: "))
  
  if (opcion %in% 1:4) {
    filas_A <- as.integer(readline("Ingrese el número de filas de la primera matriz: "))
    columnas_A <- as.integer(readline("Ingrese el número de columnas de la primera matriz: "))
    cat("Ingrese los elementos de la primera matriz:\n")
    A <- matrix(scan(n = filas_A * columnas_A), nrow = filas_A, ncol = columnas_A)
  }
  
  if (opcion %in% 1:3) {
    filas_B <- as.integer(readline("Ingrese el número de filas de la segunda matriz: "))
    columnas_B <- as.integer(readline("Ingrese el número de columnas de la segunda matriz: "))
    cat("Ingrese los elementos de la segunda matriz:\n")
    B <- matrix(scan(n = filas_B * columnas_B), nrow = filas_B, ncol = columnas_B)
  }
  
  resultado <- NULL
  
  switch(opcion,
         "1" = { resultado <- sumar_matrices(A, B) },
         "2" = { resultado <- restar_matrices(A, B) },
         "3" = { resultado <- multiplicar_matrices(A, B) },
         "4" = { resultado <- inversa_matriz(A) },
         "5" = { cat("Saliendo del programa...\n"); return(NULL) },
         { cat("Opción inválida. Intente de nuevo.\n") }
  )
  
  if (!is.null(resultado)) {
    cat("\nResultado:\n")
    print(resultado)
  }
  
  menu() # Llamada recursiva al menú para realizar otra operación
}

# Ejecutar el menú
menu()
