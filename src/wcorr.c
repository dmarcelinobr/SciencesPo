/* wcorr.c */
  #include <R.h>
  #include <Rinternals.h>
  #include <Rmath.h>


  /* Simple EXPressions from R
x is a matrix, y is a matrix, w is a vector
all have the same number of rows
the output correlation matrix has the variables from x as rows
and the variables from y as columns */

  SEXP wcorr(SEXP x, SEXP y, SEXP w){
    /* i rows, j columns, k index of respondent
    npair = number of complete observations
    ncx = number of columns of x
    ncy = number of columns of y */

      int i, j, k, n, ncx, ncy;
    double sumx, sumy, sumxy, meanx, meany, sdx, sdy, sumx2, sumy2, cor, npair;
    SEXP mat;

    /*  get dimensions of data */
      n = INTEGER(getAttrib(x, R_DimSymbol))[0];
    ncx = INTEGER(getAttrib(x, R_DimSymbol))[1];
    ncy = INTEGER(getAttrib(y, R_DimSymbol))[1];

    PROTECT(x = coerceVector(x, REALSXP));
    PROTECT(y = coerceVector(y, REALSXP));
    PROTECT(w = coerceVector(w, REALSXP));
    double *xx = REAL(x), *yy = REAL(y), *ww = REAL(w);

    /* allocate space for output matrix */
      PROTECT(mat = allocMatrix(REALSXP, ncx, ncy));

    /* for each i and j calculate each correlation and save in matrix mat */
      for (i = 0; i < ncx; i++){
        for (j = 0; j < ncy; j++){
          npair = sumx = sumy = sumxy = sumx2 = sumy2 = 0;

          /* for loop to calculate sums, the for loop is needed as we have a check for pairwise missing: ISNAN  */
            for (k = 0; k < n; k++){
              if (!ISNAN(xx[k+i*n]) && !ISNAN(yy[k+j*n])){
                npair += ww[k];
                sumx += ww[k]*xx[k+i*n];
                sumy += ww[k]*yy[k+j*n];
                sumxy += ww[k]*xx[k+i*n]*yy[k+j*n];
              }
            }

          meanx = sumx / npair;
          meany = sumy / npair;

          for (k = 0; k < n; k++){
            if (!ISNAN(xx[k+i*n]) && !ISNAN(yy[k+j*n])){
              sumx2 += ww[k]*xx[k+i*n]*xx[k+i*n] - 2*ww[k]*xx[k+i*n]*meanx + ww[k]*meanx*meanx;
              sumy2 += ww[k]*yy[k+j*n]*yy[k+j*n] - 2*ww[k]*yy[k+j*n]*meany + ww[k]*meany*meany;
            }
          }

          sdx = sqrt(sumx2/(npair-1));
          sdy = sqrt(sumy2/(npair-1));

          /*  final correlation */
            cor = (sumxy - npair*meanx*meany)/((npair-1)*sdx*sdy);

          REAL(mat)[i+j*ncx] = cor;
        }
      }

    UNPROTECT(4);

    return mat;
  }
