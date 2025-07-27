# Matrix Addition
matAdd <- function(A, B) {
    if (any(dim(A) != dim(B))) {
        stop("Matrices must have the same dimensions for addition")
    }
    C <- matrix(0, nrow = nrow(A), ncol = ncol(A))

    for (i in 1:nrow(A)) {
        for (j in 1:ncol(A)) {
            C[i, j] <- A[i, j] + B[i, j]
        }
    }
    return(C)
}

# Matrix Subtraction
matSub <- function(A, B) {
    if (any(dim(A) != dim(B))) {
        stop("Matrices must have the same dimensions for subtraction")
    }

    C <- matrix(0, nrow = nrow(A), ncol = ncol(A))

    for (i in 1:nrow(A)) {
        for (j in 1:ncol(A)) {
            C[i, j] <- A[i, j] - B[i, j]
        }
    }
    return(C)
}

# Matrix-Scalar Multiplication
matScalarMul <- function(A, scalar) {
    if (!is.numeric(scalar) || length(scalar) != 1) {
        stop("The multiplier must be a single numeric scalar")
    }
    
    C <- matrix(0, nrow = nrow(A), ncol = ncol(A))
    
    for (i in 1:nrow(A)) {
        for (j in 1:ncol(A)) {
            C[i, j] <- A[i, j] * scalar
        }
    }
    return(C)
}

# Matrix Multiplication
matMul <- function(A, B) {
    if (!is.matrix(A) || !is.matrix(B)) {
        stop("Both A and B must be matrices")
    }
    if (ncol(A) != nrow(B)) {
        stop("Incompatible dimensions : ncol(A) must be equal to nrow(B)")
    }

    C <- matrix(0, nrow = nrow(A), ncol = ncol(B))
    
    for (i in 1:nrow(A)) {
        for (j in 1:ncol(B)) {
            sum <- 0
            for (k in 1:ncol(A)) {
                sum <- sum + (A[i, k] * B[k, j])
            }
            C[i, j] <- sum
        }
    }
    return(C)
}

# Matrix Transpose
matTrans <- function(A) {
    C <- matrix(0, nrow = ncol(A), ncol = nrow(A))

    for (i in 1:nrow(A)) {
        for (j in 1:ncol(A)) {
        C[j, i] <- A[i, j]
        }
    }
    return(C)
}

# Matrix Determinant
matDet <- function(A) {
    if (!is.matrix(A) || nrow(A) != ncol(A)) {
        stop("Matrix must be Square for Det Calc")
    }
    if (nrow(A) == 1) {
        return(A[1, 1])
    }
    if (nrow(A) == 2) {
        return((A[1, 1] * A[2, 2]) - (A[1, 2] * A[2, 1]))
    }
    det <- 0
    for (i in 1:ncol(A)) {
        subMat <- A[-1, -i]
        det <- det + ((-1)^(1 + i)) * A[1, i] * matDet(subMat)
    }
    return(det)
}

# Identity Matrix
matIden <- function(n) {
    if (!is.numeric(n) || n <= 0 || n != floor(n)) {
        stop("Input must be a positive integer")
    }
    C <- matrix(0, nrow = n, ncol = n)
    for (i in 1:n) {
        C[i, i] <- 1
    }
    return(C)
}

# Print Matrix
matPrint <- function(A) {
    if (!is.matrix(A)) {
        stop("Matrix Input Only")
    }
    for (i in 1:nrow(A)) {
        for (j in 1:ncol(A)) {
            cat(sprintf("%4.f", A[i, j]), " ")
        }
        cat("\n")
    }
}

A = matrix(nrow=2, ncol=4, data=1:8)
cat("A = \n")
matPrint(A)

B = matrix(nrow=2, ncol=4, data=8:1)
cat("\nB = \n")
matPrint(B)

C = matAdd(A, B)
cat("\nC = (A+B) = \n")
matPrint(C)

C = matSub(A, B)
cat("\nC = (A-B) = \n")
matPrint(C)

tryCatch({
    C = matMul(A, B) # matMul(A, matTrans(B))
    cat("\nC = (A * B) \n")
    matPrint(C)
}, error = function(e) {
    cat("\nAn error occurred :", conditionMessage(e), "\n")
})

C = matScalarMul(A, 5)
cat("\nC = (5 * A) = \n")
matPrint(C)