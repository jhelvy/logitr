##
# @file  quasiRNG.R
# @brief R file for all quasi RNGs
#
# @author Christophe Dutang
# @author Diethelm Wuertz
#
#
# Copyright (C) 2009, Christophe Dutang,
# Diethelm Wuertz, ETH Zurich.
# All rights reserved.
#
# The new BSD License is applied to this software.
# Copyright (c) 2009 Christophe Dutang, Diethelm Wuertz.
# All rights reserved.
#
#      Redistribution and use in source and binary forms, with or without
#      modification, are permitted provided that the following conditions are
#      met:
#
#          - Redistributions of source code must retain the above copyright
#          notice, this list of conditions and the following disclaimer.
#          - Redistributions in binary form must reproduce the above
#          copyright notice, this list of conditions and the following
#          disclaimer in the documentation and/or other materials provided
#          with the distribution.
#          - Neither the name of the ETH Zurich nor the names of its
#          contributors may be used to endorse or promote
#          products derived from this software without specific prior written
#          permission.
#
#      THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#      "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#      LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#      A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#      OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#      SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#      LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#      DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#      THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#      (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#      OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#
#############################################################################
### quasi random generation
###
###     R functions
###


### quasi random generation ###

torus <- function(n, dim = 1, prime, init = TRUE, mixed = FALSE, usetime = FALSE, normal=FALSE)
{
## Check arguments
        if(n <0 || is.array(n) || !is.numeric(n))
                stop("invalid argument 'n'")
        if(dim < 1 || length(dim) >1)
                stop("invalid argument 'dim'")
        if(!is.logical(usetime))
                stop("invalid argument 'mixed'")
        if(!is.logical(mixed))
                stop("invalid argument 'mixed'")

        if(missing(prime))
                prime <- NULL
        else
        {
                if(any(prime < 0) || !is.vector(prime))
                        stop("invalid argument 'prime'")

                dim <- length(prime)
                prime <- as.integer( prime )
        }

## Restart Settings:
    if(init)
        .setrandtoolboxEnv(.torus.seed = list(offset = 1))
  if(!exists(".torus.seed", envir=.randtoolboxEnv, mode="list"))
    stop("Torus algorithm not initialized.")

## Compute
    nb <- ifelse(length(n)>1, length(n), n)
    startpt <- .getrandtoolboxEnv(".torus.seed")$offset

    res <- .Call("doTorus", nb, dim, prime, startpt, mixed, usetime)

## Normal transformation
    if(normal)
        res <- qnorm(res)

## For the next numbers save (if init=FALSE)
    .setrandtoolboxEnv(.torus.seed = list(offset = startpt+nb))

## Result
        if(dim == 1)
                as.vector(res)
        else
                as.matrix(res)
}

get.primes <- function(n)
{
  n <- min(n,100000)
  .C("get_primes",as.integer(n),integer(n))[[2]]
}

halton <- function (n, dim = 1, init = TRUE, normal = FALSE, usetime = FALSE)
{
# A function implemented by Diethelm Wuertz

    if(n < 0 || is.array(n) || !is.numeric(n))
        stop("invalid argument 'n'")
    if(dim < 1 || length(dim) >1)
        stop("invalid argument 'dim'")


# Description:
#   Uniform Halton Low Discrepancy Sequence

# Details:
#   DIMENSION : dimension unlimited?
#       N : LD numbers to create

# FUNCTION:
    if(usetime)
        start <- as.numeric(Sys.time())
    else
        start <- 0

# Restart Settings:
    if (init)
        .setrandtoolboxEnv(.halton.seed = list(base = rep(0, dim), offset = start))
  if(!exists(".halton.seed", envir=.randtoolboxEnv, mode="list"))
    stop("Halton algorithm not initialized.")

  rngEnv <- .getrandtoolboxEnv(".halton.seed")

# Generate:
    qn <- numeric(ifelse(length(n)>1, length(n), n) * dim)

# SUBROUTINE HALTON(QN, N, DIMEN, BASE, OFFSET, INIT, TRANSFORM)
    result <- .Fortran("halton",
                          qn= as.double( qn ),
                          n= as.integer( ifelse(length(n)>1, length(n), n) ),
                          dim= as.integer( dim ),
                          base= as.integer( rngEnv$base ),
                          offset= as.integer( rngEnv$offset ),
                          init= as.integer( init ),
                          trans= as.integer( 0 ),
                          PACKAGE = "randtoolbox")

# For the next numbers save (if init=FALSE)
    .setrandtoolboxEnv(.halton.seed = result[c("base", "offset")])

# Deviates:
    result <- matrix(result[["qn"]], ncol = dim)

## Normal transformation
    if(normal)
    result <- qnorm(result)

# Return Value:
    if(dim == 1)
        as.vector(result)
    else
        as.matrix(result)
}


runif.halton <- halton


sobol <- function (n, dim = 1, init = TRUE, scrambling = 0, seed = 4711, normal = FALSE)
{
# A function implemented by Diethelm Wuertz
    if(n <0 || is.array(n) || !is.numeric(n))
        stop("invalid argument 'n'")
    if(dim < 1 || dim > 1111 || length(dim) >1)
        stop("invalid argument 'dim'")
    if( !any(scrambling == 0:3) )
        stop("invalid argument 'scrambling'")


# Description:
#   Uniform Sobol Low Discrepancy Sequence

# Details:
#   DIMENSION : dimension <= 1111
#           N : LD numbers to create
#  SCRAMBLING : One of the numbers 0,1,2,3
#

# FUNCTION:


# Restart Settings:
    if (init)
        .setrandtoolboxEnv(.runif.sobol.seed =
      list(quasi = rep(0, dim), ll = 0, count = 0, sv = rep(0, dim*30), seed = seed))
  if(!exists(".runif.sobol.seed", envir=.randtoolboxEnv, mode="list"))
    stop("Sobol algorithm not initialized.")

# Generate:
    qn = numeric(ifelse(length(n)>1, length(n), n) * dim)

# SSOBOL(QN,N,DIMEN,QUASI,LL,COUNT,SV,IFLAG,SEED,INIT,TRANSFORM)
  result <- .Fortran("sobol",
            as.double( qn ),
            as.integer( ifelse(length(n)>1, length(n), n) ),
            as.integer( dim ),
            as.double ( .getrandtoolboxEnv(".runif.sobol.seed")$quasi ),
            as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$ll ),
            as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$count ),
            as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$sv ),
            as.integer( scrambling ),
            as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$seed ),
            as.integer( init ),
            as.integer( 0 ),
            PACKAGE = "randtoolbox")

# For the next numbers save (if init=FALSE)
    .setrandtoolboxEnv(.runif.sobol.seed = list(quasi = result[[4]], ll = result[[5]],
                                             count = result[[6]], sv = result[[7]],
                                             seed = result[[9]]))

# Deviates:
    result <- matrix(result[[1]], ncol = dim)

    if(any(result >= 1))
      warning("A call to sobol() raised an error by generating numerics greater or equal than 1.")

## Normal transformation
    if(normal)
    result <- qnorm(result)

# Return Value:
    if(dim == 1)
        as.vector(result)
    else
        as.matrix(result)
}


runif.sobol <- sobol

