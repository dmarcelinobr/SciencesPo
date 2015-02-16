/*============================================================================*\
 *
 * ddirichlet_log_vector ... computes the log-densities for a vector of alphas
 * ddirichlet_log_matrix ... computes the log-densities for a matrix of alphas
 * 
 * y ........ dirichlet-distributed matrix
 * alpha .... vector or matrix of alpha-values
 * rc ....... vector of number of rows and columns
 *
\*============================================================================*/



#include<R.h>
#include<Rinternals.h>
#include<Rmath.h>





SEXP ddirichlet_log_vector(SEXP y, SEXP alpha, SEXP rc){
  // helpers
  double a_sum = 0.0, aux = 0.0, norm_const;

  // pointers
  SEXP real_y = PROTECT(coerceVector(y, REALSXP));
  double *p_y = REAL(real_y);

  SEXP int_rc = PROTECT(coerceVector(rc, INTSXP));
  int v_r = INTEGER(int_rc)[0];
  int v_c = INTEGER(int_rc)[1];

  SEXP real_alpha = PROTECT(coerceVector(alpha, REALSXP));
  double *p_alpha = REAL(real_alpha);
  
  // check input
  if(length(int_rc) != 2) error("wrong specification of rc");
  if(length(y) != v_r * v_c) error("y does not match r and c");
  if((length(y)/v_r) != length(alpha)) error("alpha does not match y");
  
  // output vector and pointer
  SEXP result = PROTECT(allocVector(REALSXP, v_r));
  double *p_result = REAL(result);
  
  // computing the norming constant
  for(int col = 0; col < v_c; ++col){
    R_CheckUserInterrupt();
    a_sum += p_alpha[col];
    aux   += lgammafn(p_alpha[col]);
  }
  norm_const = lgammafn(a_sum) - aux;

  // computing densities
  for(int row = 0; row < v_r; ++row){
    R_CheckUserInterrupt();
    p_result[row] = 0.0;
    for(int col = 0; col < v_c; ++col){
      p_result[row] += ( p_alpha[col] - 1.0 ) * log( p_y[row + col * v_r] );
    }
    p_result[row] += norm_const;
  }
  
  // unprotect output vector and return
  UNPROTECT(4);
  return result;
}





SEXP ddirichlet_log_matrix(SEXP y, SEXP alpha, SEXP rc, SEXP alpha_rc){
  SEXP int_rc = PROTECT(coerceVector(rc, INTSXP));
  int v_r = INTEGER(int_rc)[0];
  int v_c = INTEGER(int_rc)[1];

  SEXP int_alpha_rc = PROTECT(coerceVector(alpha_rc, INTSXP));

  // pointers
  SEXP real_y = PROTECT(coerceVector(y, REALSXP));
  double *p_y = REAL(real_y);

  SEXP real_alpha = PROTECT(coerceVector(alpha, REALSXP));
  double *p_alpha = REAL(real_alpha);
  
  // check input
  if(length(rc) != 2) error("wrong specification of rc");
  if(length(alpha_rc) != 2) error("wrong specification of alpha_rc");
  if(length(y) != v_r * v_c) error("y does not match r and c");
  if(length(y) != length(alpha)) error("alpha does not match y");
  if((v_r != INTEGER(int_alpha_rc)[0]) || (v_c != INTEGER(int_alpha_rc)[1])) error("dimensions do not match");
  
  // output vector and pointer
  SEXP result = PROTECT(allocVector(REALSXP, v_r));
  double *p_result = REAL(result);

  double a_sum = 0.0;
  int mat_el = 0;

  for(int row = 0; row < v_r; ++row){
    R_CheckUserInterrupt();
    a_sum = 0.0;
    p_result[row] = 0.0;
    for(int col = 0; col < v_c; ++col){
      mat_el = row + col * v_r;
      a_sum += p_alpha[mat_el];
      p_result[row] += ( p_alpha[mat_el] - 1.0 ) * log( p_y[mat_el] ) - lgammafn( p_alpha[mat_el] );
    }
    p_result[row] += lgammafn(a_sum);
  }
  
  UNPROTECT(5);
  
  return result;
}
