#include <Rcpp.h>

#include <Demeter/src/ExceptionGuard.h>

using namespace Rcpp;

// [[Rcpp::export()]]
void RcppRcppPrjTmplDummy()
{
    EXCEPTION_GUARD();

    R_ShowMessage("Hello RcppPrjTmpl");

    ON_EXCEPTION();
}
