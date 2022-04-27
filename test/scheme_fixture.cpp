#include "scheme_fixture.hpp"

#include "io/write.hpp"

using namespace insider;

testing::AssertionResult
scheme_fixture::equal(insider::ptr<> x, insider::ptr<> y) {
  if (insider::equal(ctx, x, y))
    return testing::AssertionSuccess();
  else
    return testing::AssertionFailure()
      << insider::datum_to_string(ctx, x)
      << " is not equal? to "
      << insider::datum_to_string(ctx, y);
}
