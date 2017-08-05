hledger visualiser
==================

This is a little tool I've been writing to visualise
my [hledger](http://hledger.org/) records.  Tables and textual reports
are nice but sometimes visualisations, such as graphs, can really give
you new insights into your data.

![Cashflow Screenshot](screenshot.png)

**Disclaimer:** This is primarily written for me, by me.  It probably
won't work if you have a more financially complicated life than me.
The following things definitely don't work, and there may be more
things that accidentally don't work:

1. Postings with an implicit amount which involves multiple
   commodities.  For example:

    ```
    2017/07/30 I am very international and have multiple currencies
        assets:cash:paypal  -20 GBP
        assets:cash:paypal  -10 EUR
        assets:cash:bank:checking
    ```

2. Postings which do not involve your *default commodity* (see the
   example config file) at all.  So if my default commodity is not
   "CAD", this would not work:

     ```
     2017/07/30 I am currently in Canada
         expenses:syrup  30 CAD
         assets:cash:wallet
     ```

     On the other hand, if I paid with my UK debit card which did the
     currency conversion for me, this would be fine:

     ```
     2017/07/30 I am currently in Canada
         expenses:syrup  30 CAD @@ £19
         assets:cash:bank:checking
     ```

3. Asset or expense accounts with negative balances; liability or
   income accounts with positive balances.

But if you just have a fairly simple financial life where you do
everything in one currency (or only buy and sell other commodities in
your normal currency), this should work for you.


Building
--------

### Backend

Build and run with `stack`:

```bash
cd backend
stack build
stack exec backend ../config.example.yaml
```

If no argument is given, a file `./config.yaml` is tried.

### Frontend

In this section, `$WEBDIR` refers to the directory you specified in
your configuration file to serve static files from.

1. Make the directories:

    ```bash
    mkdir -p $WEBDIR/{fonts,vendor}
    ```

2. Fetch the dependencies (save all in `$WEBDIR/vendor`):

    - `highstock.js` (version 5) from https://www.highcharts.com
        **Note**: Highcharts is only free for non-commercial use.
    - `jquery.min.js` (version 3) from https://jquery.com
    - `mustache.min.js` (version 2) from https://github.com/janl/mustache.js
    - `tether.min.js` (version 1) from http://tether.io
    - `font-awesome` (version 4) from http://fontawesome.io
    - `bootstrap` (version 4) from https://v4-alpha.getbootstrap.com

3. Fetch the Calluna Sans regular font face (the free one) from https://www.fontspring.com/fonts/exljbris/calluna-sans,
   saved as `$WEBDIR/fonts/CallunaSansRegular.woff2`

4. Finally, copy over the frontend files:

    ```bash
    cp frontend/* $WEBDIR
    ```

Think this is a hassle?  I'm planning to add an option to just use CDNs for all these dependencies.
