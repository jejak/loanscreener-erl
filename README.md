# Loan Screener (Erlang)

It is the erlang implementation of Loan Screener project https://github.com/jejak/loanscreener.

## Summary
Loan Screener is a proto Command-line Interface program to screen defaulting loans in a banking infrastructure taking the loan data, the loan credit policy data and the loan equity porfolio share prices (market data) from the internet from one or more banking subsidiaries.

The program
- takes url-s of Json files of market data, loan data and credit policies as command-line parameters and
- generates json output on STDOUT showing which loans are now in default
- if any error occured it would be reported on STDERR  

**Notes**
* Loans contain:
    - a reference to the credit policy
    - a loan id
    - an amount which the borrower has borrowed.
    - a series of equity positions, consisting of an ISIN (id) and the number of shares owned.
* Credit policies consist of a policy id, a currency and a minimum acceptable equity price.
  - Any equity whose price is below this (per share) is ignored for the purposes of calculating the collateral of the loan.
  - Any equity whose currency does not match is ignored.
* The value of the collateral is number of eligible shares x eligible price of share. If the credit policy deems shares ineligible they are omitted from the calculation.
* A loan is considered in default if its eligible

**Example output**

````````````````````
$ loanscreener <url-of-marketdata-json> \
               <url-of-loandata-json> \
               <url-of-creditpolicy-json>

[
  {
    "id": "loan1234",
    "creditpolicy": "policy1234",
    "amount": 10000,
    "eligible_collateral": 9999,
  },
  ...
]
````````````````````

## Design

* the choice of programming language now is: erlang
* use the httpc lib of Erlang Standard Library to do http get to retrieve the market data, loan data and credit policies data from the internet
* use the rebar3/hex jsx package
  - _to parse json format_: jsx:decode(binary()) -> term()  
  - _or to create json_: jsx:encode(term()) -> binary()

### Call Follow
- get the content of market data, loan data and credit policy json
- process loan data (which is an array of loans) for default checks
- for all loans calculates the eligible collateral value
- do eligibilty check for the loan equity positions according to the loan credit policy (eligibility check for currency and price threshold)
- establishes if loan is in default
    * loan in default if:
        - *eligible-calculated-loan-collateral-value> < loan-amount*
- generate report from all collected defaulting loans
- format report entries as per requirement on STDOUT
- report any error occured on STDERR

*Note: if there is no any eligible positon for a loan then this loan is not regarded as defaulting loan*

### Unit Test
- used erlang eunit application for unit testing and
- used rebar3/hex Meck package to mock remote HTTP file server so that the test cases can run without a real internet connection   
- the test checks error and success senerios
- if report expacted the reporting entries are checked if they are complete in the output json

## Notation

*The folder where the git project has been cloned will be referred as **Code** from now*.

## To build the program

### Run rebar compile & escriptize  

To build the program run the following commands.

````````````````````
> cd Code
> rebar3 compile
> rebar3 escriptize
````````````````````

## To run the Loan Screener program

### Do the build step

Do the above build step

### See usage

````````````````````````
Usage: loanscreener <url-of-marketdata-json> <url-of-loandata-json> <url-of-creditpolicy-json>
       loanscreener http://ws.jenojakab.com/files/marketdata.json \
                     http://ws.jenojakab.com/files/loandata.json \
                     http://ws.jenojakab.com/files/creditpolicy.json

Options:
--help        Prints this help
````````````````````````

### Run these commands

````````````````````````
> cd Code
> loanscreener <url-of-marketdata-json> <url-of-loandata-json> <url-of-creditpolicydata-json>
````````````````````````

## To Run the Unit Test
### Run these commands

````````````````````````
> cd Code
> rebar3 eunit
````````````````````````

The test results would genereted on the console.

### Example test results

````````````````````````
> rebar3 eunit
===> Verifying dependencies...
===> Linking _build/default/lib/jsx to _build/test/lib/jsx
===> Linking _build/default/lib/meck to _build/test/lib/meck
===> Compiling loanscreener_cli
===> Performing EUnit tests...
....

Top 4 slowest tests (0.048 seconds, 4.1% of total time):
  loanscreener_test:loanscreen_test_/0: Sample data from files test - should return two loan alerts
    0.032 seconds
  loanscreener_test:loanscreen_test_/0: Two loans one position not eligible by cp threshold test - should return one loan alert
    0.016 seconds
  loanscreener_test:loanscreen_test_/0: Two loans two positions are not eligible by cp currency and threshold - should return one loan alert
    0.000 seconds
  loanscreener_test:loanscreen_test_/0: Two loans one position not eligible by cp threshold test - should return two loan alerts
    0.000 seconds

Finished in 1.185 seconds
4 tests, 0 failures
````````````````````````
