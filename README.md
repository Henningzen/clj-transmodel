# no.jansenh/clj-transmodel

Public transport standards NeTEx, SIRI (ET, SX, VM) under CEN/TC 278, Transmodel reference model (EN 12896).
We are creating a highly opinionated library of parsers for the NeTEx and SIRI standards.

## Usage

Invoke a library API function from the command-line:

    $ clojure -X no.jansenh.clj-transmodel/foo :a 1 :b '"two"'
    {:a 1, :b "two"} "Hello, World!"

Run the project's tests (they'll fail until you edit them):

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
