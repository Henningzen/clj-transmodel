# no.jansenh/clj-transmodel

Public transport standards NeTEx, SIRI (ET, SX, VM) under CEN/TC 278, Transmodel reference model (EN 12896).
We are creating a highly opinionated library of parsers for the NeTEx and SIRI standards.

## Installation

Latest develop stable release is 0.2.0

Add this library to your `deps.edn`:

```clojure
{jansenh/clj-transmodel {:mvn/version "0.1.0"}}
```

## Usage

Use it in your Clojure code:

```clojure
(require '[jansenh.clj-transmodel :as tm])
(tm/your-function arg1 arg2)
```

Run the project's tests

	$ clojure -T:build test

Run the project's CI pipeline and build a JAR (this will fail until you edit the tests to pass):

    $ clojure -T:build ci

This will produce an updated `pom.xml` file with synchronized dependencies inside the `META-INF`
directory inside `target/classes` and the JAR in `target`. You can update the version (and SCM tag)
information in generated `pom.xml` by updating `build.clj`.

Install it locally (requires the `ci` task be run first):

    $ clojure -T:build install

Deploy it to Clojars -- needs `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment
variables (requires the `ci` task be run first):

    $ clojure -T:build deploy

Your library will be deployed to no.jansenh/clj-transmodel on clojars.org by default.


## Notebook usage

The repository has a Clay notebook from the Noj family of data-science tools. Go read the manual.

1. Fire up the notebook/netex namespace in your REPL
2. Invoce the notebook start command. You will find it at the bottom of the file in a comment section
3. Your browser will open a Clay page and instructions will appear.

The notebook assume NeTEx xml files hosted in some data folder on you local system. Contact me for details on how to obtain these datasets. Moving forward, a download feature will appear in the server module.

## License

Copyright © 2025 Henning Jansen

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.

## Contact
For any questions or feedback, please contact:

Henning Jansen
henning.jansen@jansenh.no
GitHub: henningzen
