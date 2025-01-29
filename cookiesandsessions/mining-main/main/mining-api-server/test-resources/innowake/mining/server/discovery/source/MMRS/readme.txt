
CreateBat
  - up to one argument: the mvn command to execute (defaults to dependency:sources -U)
  - creates a file named 'update.bat' updating all projects in the workspace (open and closed)
  - this works for mixed versions within one workspace
  
CreatSuperPom
  - no args
  - creates a file named 'pom.xml' with dependecies to all maven projects in the workspace (open and closed)
  - this only works for compatible versions in workspace (e. g. two projects with different versions would fail in maven call)

Launch Configurations
  - requires launch variable 'M2_EXECUTABLE_3_9_5'
    - example mac: /Users/hermann.roescheisen/Development/maven3/3.9.5/bin/mvn
    - example win: K:/3.9.5/bin/mvn.cmd

Regular Expression to remove references to local javadocs from the classpath after 'mvn eclipse':
  - Search in .classpath file
  - Enable option 'Regular expression'
  - Search for: ^(\s*)<(classpathentry [^>]+)>\s*<attributes>\s*<attribute[^>]*name="javadoc_location"[^>]*/>\s*</attributes>\s*</classpathentry>
  - Replace with: $1<$2/>
