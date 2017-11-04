#include <Rcpp.h>
#include <iterator>
using namespace Rcpp;
// [[Rcpp::export]]
void insertion_sort(NumericVector &x) {
  NumericVector::iterator iter1 = x.begin(), iter3 = x.begin(), iter2, iter4;
  ++iter1;
  --iter3;
  for(;iter1!=x.end();++iter1) {
    iter2 = iter1;
    --iter2;
    for(;iter2!=iter3;--iter2) {
      iter4 = iter2;
      ++iter4;
      if(*iter2 > *iter4) {
        std::swap(*iter2, *iter4);
      }
    }
  }
}

/*** R
array <- rnorm(10000)
start.time <- Sys.time()
insertion_sort(array)
end.time <- Sys.time()
print(end.time - start.time)


*/
