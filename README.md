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
<<<<<<< HEAD
=======
* Note: sbt test-compile does not like it if a custom scala compiler is located outside the project directory. Place virtualized-scala in the Delite directory
* Compile virtualized-scala using `ant`
>>>>>>> b51ff14723ded869ceb7e0f203ce220be0ddd715

### Virtualization Light Modular Staging library
* [Virtualization LMS Core](http://github.com/TiarkRompf/virtualization-lms-core)

### Simple Build Tool
* [Simple Build Tool](http://code.google.com/p/simple-build-tool/)
* You will want to create some kind of launcher for sbt. See sbt [setup instructions](http://code.google.com/p/simple-build-tool/wiki/Setup).

### Source code
* [Delite](http://github.com/stanford-ppl/Delite)

You can grab the repository with the following command:

`git clone [REPOSITORY URL]`

Project setup instructions
--------------------------

Until we have a project file checked into the repository, you will have to create the project yourself.

### Short and sweet instructions

1. Create a new project with the Project Files location set to the Delite directory
2. Add a Project Library scala-virtualized with Classes scala-compiler.jar and scala-library.jar. These are found under scala-virtualized/lib
3. Add modules from scratch, with directories set to Delite/dsls/optiml, Delite/framework, Delite/runtime, Delite/tests, Virtualization-lms-core
4. Add Scala facets to each module, with compiler library scala-virtualized/lib/scala-compiler.jar
5. Add the project library dependency scala-virtualized to all modules
6. Add module dependencies as follows:

    framework depends on virtualization-lms-core

    optiml depends on framework, runtime, and virtualization-lms-core

    runtime depends on framework, virtualization-lms-core

    tests depends on framework, optiml, runtime, virtualization-lms-core

    virtualization-lms-core only depends on scala-virtualized.
            
7. Test the project structure with Build->Make Project

8. Create a local.properties file inside the Delite and the virtualization-lms-core folder with the following lines. Select the appropriate line based on your system and replace with appropriate paths. Absolute paths must be used if the scala-virtualized directory is outside of either project.

        #Project local properties
        # Windows:
        scala.virtualized.home=C:/PPL/scala-virtualized
        # Unix:
        scala.virtualized.home=/PPL/scala-virtualized

9. Generate the `scala-virtualized` compiler by running `ant` in the `scala-virtualized` directory

10. Generate the virtualization-lms-core package so the Delite sources can see it by running the command `publish-local`, either from the command line by:

    sbt publish-local

11. You should be able to run `sbt` from the Delite directory.

12. Update dependencies by running the `sbt` command `update`

### Step by step

1. Open up IDEA
2. Go to File -> New Project
3. Select `Create Java project from existing sources`

4. Name the project Delite or something similar
5. Select the project files location as the Delite repository you just downloaded. For example: `C:\Code\PPL\Delite`

6. Select `Do not create source directory`

7. Continue pressing next through the libraries dialog. If it asks for a JDK, add a JDK to the project.

8. Go to File->Project Structure
9. Select Libraries under -Project Settings-
10. Click the Add button
11. Name the library `scala-virtualized` or something similar
12. Add the new library to any modules you have already defined
13. Click `Attach Classes...`
14. Browse to the lib directory under the scala-virtualized directory, and add scala-compiler.jar and scala-library.jar

15. Create the project modules
16. Select Modules under -Project Settings-
17. Click the Add button, then click Module
18. Select Create module from scratch
19. It will ask you for the module path and name. Click ... (browse) on content root.
20. Select the content directory. Let's start with Delite/dsls/optiml
21. IDEA should automatically fill in the data for you. Click Next
22. IDEA should have automatically selected Create source directory. Click Next
23. Do not select desired technology. Click Finish.

24. Select your new module and either right click -> New -> Scala, or click Add -> New -> Scala
25. Select the Scala facet that appears.
26. Add a new Compiler plugin (do this only one time for the project). This should be under scala-virtualized/lib/scala-compiler.jar
27. Select scala-virtualized under Compiler library. If nothing is under Compiler library, click OK on the Project Structure dialog and open up project structure again.

28. Create new modules for Delite/framework, Delite/runtime, Delite/tests, and virtualization-lms-core

29. Now add dependencies. Click on a module, say framework, and click Dependencies.
30. Add the project library scala-virtualized.
31. Add the module dependency virttualization-lms-core
32. Add module dependencies as follows:

    optiml depends on framework, runtime, and virtualization-lms-core

    runtime depends on framework, virtualization-lms-core

    tests depends on framework, optiml, runtime, virtualization-lms-core

    virtualization-lms-core only depends on scala-virtualized.

33. You should be able to build successfully with Build->Make Project
    
34. Create a local.properties file inside the Delite folder with the following lines. Select the appropriate line based on your system and replace with appropriate paths. Absolute paths must be used if the scala-virtualized directory is outside of the project directory.

        #Project local properties
<<<<<<< HEAD
        # Windows:
        scala.virtualized.home=C:/PPL/scala-virtualized
        # Unix:
        scala.virtualized.home=/PPL/scala-virtualized
=======
        scala.virtualized.home=scala-virtualized/build/pack
        virtualization_lms_core.home=virtualization-lms-core
>>>>>>> b51ff14723ded869ceb7e0f203ce220be0ddd715
    
35. Create a local.properties file inside the virtualization-lms-core folder with the following lines. Replace the paths with ones that apply to your system. Absolute paths must be used if the scala-virtualized directory is outside of the project directory.

        #Project local properties
<<<<<<< HEAD
        # Windows:
        scala.virtualized.home=C:/PPL/scala-virtualized
        # Unix:
        scala.virtualized.home=/PPL/scala-virtualized
=======
        scala.virtualized.home=scala-virtualized/build/pack
>>>>>>> b51ff14723ded869ceb7e0f203ce220be0ddd715

37. Generate the `scala-virtualized` compiler by running `ant` in the `scala-virtualized` directory

38. Generate the `virtualization-lms-core` package so the Delite sources can see it by running the command `publish-local` from either the `sbt` shell or the command line with `sbt publish-local`

<<<<<<< HEAD
39. You should be able to run `sbt` from the Delite directory.
=======
    runtime depends on framework, virtualization-lims-core

    tests depends on framework, optiml, runtime, virtualization-lms-core

    virtualization-lms-core only depends on scala-virtualized.
            
7. Test the project structure with Build->Make Project

8. Create a local.properties file inside the Delite folder with the following lines. Replace the paths with ones that apply to your system

        #Project local properties
        scala.virtualized.home=scala-virtualized/build/pack
        virtualization_lms_core.home=virtualization-lms-core
    
9. Create a local.properties file inside the virtualization-lms-core folder with the following lines. Replace the paths with ones that apply to your system. Relative paths should be relative to the Delite folder. Otherwise absolute paths can be used.

        #Project local properties
        scala.virtualized.home=scala-virtualized/build/pack
>>>>>>> b51ff14723ded869ceb7e0f203ce220be0ddd715

40. Update dependencies by running the command `update`, either from the `sbt` shell or the command line with `sbt update`
