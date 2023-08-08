#include <Rcpp.h>

#include <Common/CppExt/CppExt.h>

#include <Demeter/src/ExceptionGuard.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export()]]
List RcppFindNear(NumericVector timeA, NumericVector timeB, double threshold)
{
    EXCEPTION_GUARD();

    List ret(timeA.size());
    int i = 0;

    for (auto t1 : timeA)
    {
        vector<int> v;
        auto low = lower_bound(timeB.begin(), timeB.end(), t1);
        while (low != timeB.end() && *low <= t1 + threshold)
        {
            v.push_back(low - timeB.begin() + 1);
            ++low;
        }

        ret[i++] = wrap(v);
    }

    return ret;

    ON_EXCEPTION(return List());
}

// [[Rcpp::export()]]
List RcppFindMatch(List a, List b, std::string aTimeCol, std::string bTimeCol, double threshold, std::string by)
{
    EXCEPTION_GUARD();

    CheckNMsg(a.containsElementNamed(aTimeCol.c_str()), "a doesn't have column {}", aTimeCol);
    CheckNMsg(a.containsElementNamed(by.c_str()), "a doesn't have column {}", by);
    CheckNMsg(b.containsElementNamed(bTimeCol.c_str()), "b doesn't have column {}", bTimeCol);
    CheckNMsg(b.containsElementNamed(by.c_str()), "b doesn't have column {}", by);

    auto timeA = as<NumericVector>(a[aTimeCol]);
    auto byA = as<NumericVector>(a[by]);
    auto timeB = as<NumericVector>(b[bTimeCol]);
    auto byB = as<NumericVector>(b[by]);

    vector<int> indexA;
    vector<int> indexB;
    indexA.reserve(timeA.size());
    indexB.reserve(timeA.size());

    for (int i = 0; i < timeA.size(); ++i)
    {
        auto low = lower_bound(timeB.begin(), timeB.end(), timeA[i]);
        while (low != timeB.end() && *low <= timeA[i] + threshold)
        {
            auto index = int(low - timeB.begin());
            if (byA[i] == byB[index])
            {
                indexA.push_back(i + 1);
                indexB.push_back(index + 1);
                break;
            }
            ++low;
        }
    }

    auto ret = List::create(_["a.index"] = wrap(indexA), _["b.index"] = wrap(indexB));

    return ret;

    ON_EXCEPTION(return List());
}
