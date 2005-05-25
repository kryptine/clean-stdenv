This library module is under development !

The Html library can be used to generate Html code for browsers
and to make interactive web applications.

To use it one needs a Server running on the machine.
Make sure that the protection of your machine is prepared for this kind of facility.
It is therefore a good idea to use portnumbers > 100.
The server has to understand php scripts (they normally do).

Try out the examples first and have a look how they are coded.

If you want to make your own application, the best way is just to copy one of the examples,
e.g. spreadsheet.icl.

1. Rename the copied file too yourname.icl

2. Open the file yourname.icl, the Clean IDE should start automatically.
Rename the module spreadsheet to module yourname.
Create a project (File/New Project...) and store the indicated project file as yourname.prj
Choose as environment (Environment/Everything) "Everything"
Your Clean application has to be compiled (Project/Project Options...) with "No Retun Type" option selected.
Update the project (Project/Bring Up To Date)

3. Make a copy of any php script, eg spreadsheet.php,
and rename it to yourname.php.
Edit the script and replace spreadsheet.exe by yourname.exe.

4. Open the browser and select yourname.php
It should display a page in the same way as the original application you copied.

5. Now modify the application as desired. Test the page using a browser.


Have fun,

Rinus.

========================

To do list:

Known bugs:
- don't use html command symbols or control characters in button or form names
- buttons don't work if their names contains spaces
- if states are getting very large, nothing is displayed: have to sort out whom is to blame
 
Conceptual:

- make storage of higher order function possible (involves dynamics)
- incorporate reading / writing to files

Applications:

- The following applications are under development: web shop, process algebra, family tree

