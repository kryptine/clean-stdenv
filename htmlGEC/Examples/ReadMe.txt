This is the Clean Html GEC - iData library.

This library is under development.
Is has only been tested under windows, but it should work on any other machine Clean 2.1.1 is available on.

I. *** Install as follows:


1. You need the latest Clean system (version 2.1.1 or higher) for windows which can be downloaded
from our site (www.cs.ru.nl/~clean).
Install this system. It will generate a Clean 2.1.1 folder.
Put it anywhere, e.g. on your desktop.
Read the ReadMe that comes with the installation.
In particular, you have to launch the Clean IDE once. 
It will associate files with Clean extensions to the Clean IDE. That is all.

You can work with Clean, but the html library is not yet included in the latest version,
so you have to add this library yourself (points 3 - 5)


2. Move the htmlGEC library into the folder Clean 2.1.1/Libraries.


3. Click on one of the .prj files of the html Examples, in the htmlExamples folder, e.g. spreadsheet.prj


4. Select in the Clean IDE menubar, Environment/Everything.
This option is now marked.


5. Select Environment/Edit Current ...
Now properties of the Everything environment can be set.
Click the path tab.


Append the following libraries:
{Application}\Libraries\GEC\GEC Implementation
{Application}\Libraries\htmlGEC

Now these libaries are part of the Everything environment. Projects which select this environment
will find the software modules automatically.

Installation is now complete.





II *** Running a Clean html application.

To run a Clean html application you need a server on your machine.
There is an easy way to do it, and a hard way.

You better start with the easy way:



*** Easy way

This mode is great for testing and playing.
A server is included in the html library.
The server software has been written by Paul de Mast from the Polytechnical University,
Breda, The Netherlands. Thank you very much Paul!

1. Open one of the html examples in the htmlExample folder,
just by clicking on the .prj file, take e.g. spreadsheet.prj.
The Clean IDE will be launched. 

Make sure that the project Start rule looks like:

Start world = doHtmlServer .... world

If it says

Start world = doHtml ... world

simply change doHtml into doHtmlServer.

Now you are in the easy mode.

2. Choose from the menubar Project/Update and Run (Ctrl+r)

Everyting will be compiled, and the executable will be started showing a black command line window.

The application is both the server and it includes the generator of the html pages.
You can stop it as usual by pressing the x button.

3. Start your browser (e.g. Explorer) and choose:

http://localhost/clean

You will see the effect of the chosen example.

The browser might warn for all kinds of things, but you don't have to worry. It is quite safe.



*** Hard way

Install your own favorite server on your machine.
I have used "badblue" which is free and works fine.
Make sure that the protection of your machine is prepared for this kind of facility.
Virus attacks to servers are very very likely.
I can recommend Alvast! virus protection.
If you are not a professional, be prepared for serious problems.
Inform yourself for the latest virus protection settings and utilities!
It is a good idea to use portnumbers > 100.

The server has to understand php scripts (they normally do).
In the future I will make a version that does not need php anymore... 

1. Open one of the html examples in the htmlExample folder,
just by clicking on the .prj file, take e.g. spreadsheet.prj.
The Clean IDE will be launched. 

Make sure that the project start rule looks like:

Start world = doHtml .... world

If it says

Start world = doHtmlServer ... world

simply change doHtmlServer into doHtml.

Now you are in the "hard" mode requiring a running external server.

2. Choose from the menubar Project/Bring Up To Date (Ctrl+u)

Everyting will be compiled, and an executable (.exe) will be generated.

3. The executable with its assets (e.g. pictures) will have to be put in a directory reachable from the browser.
This depends on the settings of your server.
You also have to put the php script in the same directory.
The php script foo.php will launch foo.exe.

4. Start your browser (e.g. Explorer) and select the proper directory:
Choose the php file and click.
The bowser will have to understand php.
The script will launch the Clean application.
The Clean application will produce a page after which it stops with execution.
Each change made in a page will restart the Clean application (via the php script).


*** making your own aplication from scratch.

If you want to make your own application, the best way is just to copy one of the examples,
e.g. spreadsheet.icl.

1. Rename the copied file too yourname.icl
Also copy the file back35.jpg. The default web style uses white characters so you better have a none white background.
You can of course set everything the way you like it by changing the styles...

2. Open the file yourname.icl, the Clean IDE should start automatically.
Rename the module spreadsheet to module yourname (first line in the file).
Create a project (File/New Project...) and store the indicated project file as yourname.prj
Choose as environment (Environment/Everything) "Everything".
Your Clean application has to be compiled (Project/Project Options...) with "No Return Type" option selected.

Follow the steps for compilation described above.

In "hard" mode, make a copy of any php script, eg spreadsheet.php,
and rename it to yourname.php.
Edit the script and replace spreadsheet.exe by yourname.exe.

Check if everything still works.

3. Now modify the application as desired.


Have fun,

Rinus.

========================

To do list:

Known bugs:
- don't use html command symbols or control characters in button or form names.
- buttons don't work if their names contains spaces.
- if states are getting very large, sometimes nothing is displayed. 
php has a limit of 4k for passing arguments to Clean.
I therefore want to kick out php.

Examples:
- Not all examples are completely worked out yet.
