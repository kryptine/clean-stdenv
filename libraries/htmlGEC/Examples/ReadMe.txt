This is the Clean Html GEC - iData library.

This library is under development.
Is has only been tested under windows, but it should work on any other machine Clean 2.2 is available on.

I. *** Install as follows:


1. You need the latest Clean system (version 2.2 or higher) for windows which can be downloaded
from our site (www.cs.ru.nl/~clean).
Install this system. It will generate a Clean 2.2 folder.
Put it anywhere, e.g. on your desktop.
Read the ReadMe that comes with the installation.
In particular, you have to launch the Clean IDE once. 
It will associate files with Clean extensions to the Clean IDE. That is all.

You can work with Clean, but the html library is not yet included in the latest version,
so you have to add this library yourself (points 3 - 5)


2. Move the htmlGEC library into the folder Clean 2.2/Libraries.


3. Click on one of the .prj files of the html Examples, in the htmlExamples folder, e.g. spreadsheet.prj


4. In the Clean IDE menubar, Environment/Web Applications should be selected.


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

We are working on this variant.


There will be two options:

a. Install your own server and run the Clean application as a CGI script.

b. Install a special Clean server.
This special server can have several Clean applications running as CGI subservers.
This method has as advantage that you don't need big servers running on your machine and
you don't need to be afraid of virus attacks.


We are currently testing the system.



