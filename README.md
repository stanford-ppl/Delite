Delite 2.0
==========

Dependencies
------------

### IDE
* [IDEA IntelliJ](http://www.jetbrains.com/idea/download/). You may want to configure the memory parameters in the VM options for IDEA.
* Scala plugin

### Git
* [Git for Windows](http://code.google.com/p/msysgit/)

### Ant
* [Ant](http://ant.apache.org/)

### Svn client
* [SlikSVN for Windows](http://www.sliksvn.com/en/download)

### Virtualized Scala
* [Virtualized Scala](http://github.com/TiarkRompf/scala-virtualized)
(this one is missing some things for now, get compiler from our file store)
* Compile virtualized-scala using `ant`

### Virtualization Light Modular Staging library
* [Virtualization LMS Core](http://github.com/TiarkRompf/virtualization-lms-core)

### Simple Build Tool
* [Simple Build Tool](http://code.google.com/p/simple-build-tool/)
* You will want to create some kind of launcher for sbt. See sbt [setup instructions](http://code.google.com/p/simple-build-tool/wiki/Setup).
* You may want to increase the memory available for sbt by adding something like `-Xmx1024M` to the java options when launching sbt

### Source code
* [Delite](http://github.com/stanford-ppl/Delite)

You can grab the repository with the following command:

`git clone [REPOSITORY URL]`

SBT setup instructions
----------------------

1. Ensure `scala-virtualized` is built by running `ant` in the `scala-virtualized` directory.

2. Build `virtualization-lms-core` by running the `sbt` command `publish-local`

3. Create a `local.properties` file inside the `Delite` and the `virtualization-lms-core` folder with the following lines. Select the appropriate line based on your system and replace with appropriate paths. Absolute paths must be used if the `scala-virtualized` directory is outside of either project.

        #Project local properties
        # Windows:
        scala.virtualized.home=C:/PPL/scala-virtualized/build/pack
        # Unix:
        scala.virtualized.home=/PPL/scala-virtualized/build/pack

4. Generate the `scala-virtualized` compiler by running `ant` in the `scala-virtualized` directory

5. You should be able to run `sbt` from the Delite directory.

6. Update dependencies by running the `sbt` command `update`

IDEA project setup instructions
--------------------------

Until we have a project file checked into the repository and a standardized directory layout, you will have to create the project yourself.

### Short and sweet IDEA setup instructions

1. Build `scala-virtualized` by running `ant`
2. Create a new project with the `Project Files` location set to the `Delite` directory
3. Add a Project Library `scala-virtualized` with Classes `scala-compiler.jar` and `scala-library.jar`. These are found under `scala-virtualized/build/pack/lib`
4. Add modules from scratch, with directories set to `Delite/dsls/optiml`, `Delite/framework`, `Delite/runtime`, `Delite/tests`, `virtualization-lms-core`
5. Add Scala facets to each module, with compiler library `scala-virtualized/build/pack/lib/scala-compiler.jar`
6. Add the project library dependency `scala-virtualized` to all modules
7. Add module dependencies as follows:

    `framework` depends on `virtualization-lms-core`

    `optiml` depends on `framework`, `runtime`, and `virtualization-lms-core`

    `runtime` depends on `framework`, `virtualization-lms-core`

    `tests` depends on `framework`, `optiml`, `runtime`, `virtualization-lms-core`

    `virtualization-lms-core` only depends on `scala-virtualized`
            
9. Test the project structure with `Build` -> `Make Project`

### Step by step

1. Build `scala-virtualized` by running `ant` in the `scala-virtualized` directory
2. Open up `IDEA`
3. Go to `File` -> `New Project`
4. Select `Create Java project from existing sources`

5. Name the project `Delite` or something similar
6. Select the project files location as the `Delite` repository you just downloaded. For example: `C:\Code\PPL\Delite`

7. Select `Do not create source directory`

8. Continue pressing next through the libraries dialog. If it asks for a JDK, add a JDK to the project.

9. Go to `File` -> `Project Structure`
10. Select `Libraries` under `-Project Settings-`
11. Click the `+` (add) button
12. Name the library `scala-virtualized` or something similar
13. Add the new library to any modules you have already defined
14. Click `Attach Classes...`
15. Browse to the `lib` directory under the `scala-virtualized/build/pack` directory, and add `scala-compiler.jar` and `scala-library.jar`

16. Create the project modules
17. Select Modules under -Project Settings-
18. Click the Add button, then click Module
19. Select Create module from scratch
20. It will ask you for the module path and name. Click ... (browse) on content root.
21. Select the content directory. Let's start with `Delite/dsls/optiml`
22. `IDEA` should automatically fill in the data for you. Click `Next`
23. `IDEA` should have automatically selected `Create source directory`. Click `Next`
24. Do not select a desired technology. Click `Finish`.

25. Select your new module and either right click -> `New` -> `Scala`, or click `+` (add) -> `New` -> `Scala`
26. Select the `Scala` facet that appears.
27. Add a new Compiler plugin (do this only one time for the project). This should be under `scala-virtualized/build/pack/lib/scala-compiler.jar`
28. Select `scala-virtualized` under `Compiler library`. If nothing is under `Compiler library`, click `OK` on the `Project Structure` dialog and open up the project structure dialog again.

29. Create new modules for `Delite/framework`, `Delite/runtime`, `Delite/tests`, and `virtualization-lms-core`

30. Now add dependencies. Click on a module, say `framework`, and click `Dependencies`.
31. Add the project library `scala-virtualized`.
32. Add the module dependency `virtualization-lms-core`
33. Add module dependencies as follows:

    `optiml` depends on `framework`, `runtime`, and `virtualization-lms-core`

    `runtime` depends on `framework`, `virtualization-lms-core`

    `tests` depends on `framework`, `optiml`, `runtime`, `virtualization-lms-core`

    `virtualization-lms-core` only depends on `scala-virtualized`

34. You should be able to build successfully with `Build` -> `Make Project`

Testing
--------------------------

`sbt` will automatically pick up any tests in the `tests/src` directory of any project or subproject. Currently it is configured with [ScalaTest](http://www.scalatest.org/), which supports many types of tests, including JUnit.

Test resources can be added under the `tests/resources` directory under `Delite` or any sub-project. These files will be added onto the classpath and be included in the test jar.

An example test can be found under `tests/src/BasicTest.scala`. It tests a basic Scala class and resource loading. Test output comparison can then be done by loading up the resource as a stream and comparing it to whatever output is generated.

