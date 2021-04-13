[Model version 0.6]
Key model features can also be explored in the "Simplified" version of the model which can run without extesions on NetLogo Web app (https://www.netlogoweb.org/launch)

HOW TO MAKE IT WORK:


In order to make the program work you must first initialize the NetLogo-R extension. 
Follow the steps below to set your environment. 

0) Install the R-extension in NetLogo

1) Install R https://cran.r-project.org/ (IF MISSING)

2) Open R and install the rJava package (run command:  install.packages(rJava)) 
   install also the package raster (run command: install.packages(raster)) you need this for the functions applied in NetLogo.

3) Run the following lines: 

	R.home(component = "home")
	system.file("jri", package = "rJava")

It will return two paths. 

4) Open the file "user.properties" in the directory "sources" with a text editor and change the existing paths with the one found above.
 

5) (ONLY FOR WINDOWS, if mac/linux go to 6) You must have installed Microsoft Visual C++ 2013 redistributable:
    (You should have it, but you can download it at:  https://www.microsoft.com/en-us/download/details.aspx?id=40784)

Windows requires the additional configuration step of configuring the PATH environment variable. Additionally, editing the user.properties file on Windows is slightly more difficult than on other platforms.
Configuring the PATH

To begin, determine the appropriate directory from your R installation to add to your PATH. To do this, determine where your R installation is located (here we’ll use the location C:\Program Files\R\R-), then follow these steps (USUALLY is the DEFAULT ONE).

    - Open the System Properties dialog. You can type “Environment Variable” into Cortana or navigate there through “Control Panel” > System > “Advanced system settings”.
   
    - Click the “Environment variables…” button in the lower right of the dialog.
    
    - Click the “Path” variable in the lower panel, then click the lower “Edit…” button.
    
    - Windows 10 allows you to choose “New” and enter a separate path. If you’re using Windows 7, append the value, using a semicolon to separate it from the entry before.

    - If you’re using 32-bit NetLogo, enter the location C:\Program Files\R\R-<version>\bin\i386\       #### CHANGE <version> with your R version (get it from R console with the "R.version"  command)
    - If you’re using 64-bit NetLogo, enter the location C:\Program Files\R\R-<version>\bin\x64\    

    - Choose OK, and OK again

    Log out of your user and back in or restart Windows to let the setting take affect.

(Note that you will need to update this setting if you wish to upgrade the version of R used by NetLogo.)


6) Go to one of the following paths:


    On Mac OS X: /Users/YOUR_USERNAME/Library/Application Support/NetLogo/6.1/r/
    On Windows: C:\Users\YOUR_USERNAME\AppData\Roaming\NetLogo\6.1\r\
    On Linux: /home/YOUR_USERNAME/.netlogo/6.1/r/

(For me in windows 10 worked to open the cmd and type :

cd C:\Users\<YOUR_USERNAME>\AppData\Roaming\NetLogo\<YOURNlogoVersion>\r 
start .

)

Copy paste here the file "user.properties" (or sobstitute it if already existing).


7) You are set


#################################################################

Remember to change paths in the code in order to make it work. (I need to figure out how to set them in order to make it work "outofthebox")
