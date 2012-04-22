##########################################################
##
## Generate Dashboard for Delite paper experiments
## 
##########################################################
##
##  TODO
##########
##


apps = ['gda', 'nb', 'linreg', 'kmeans', 'rbm', 'svm']#, 'lbp']
delite2_procs = [ 1, 2, 4, 8]
delite1_procs = [ 1, 2, 4, 8]
matlab1_procs = [ 1, 2, 4, 8]

#for convenience, but you can override these via script options
timesDir = "C:/Users/hchafi/Documents/Dropbox/ppl/ppl/delite/data/"
outputDir = "C:/Users/hchafi/Documents/Dropbox/ppl/ppl/delite/data/"


import os

from optparse import OptionParser
from xlwt import Workbook, easyxf
from collections import deque
import numpy

######################################
## Style section
######################################

labelStyle = easyxf(
    'font: name Arial, colour dark_blue;'
    #'borders: left thick, right thick, top thick, bottom thick;'
    #'pattern: pattern solid, fore_colour red;'
    'alignment: horizontal center;'
)

headerAppNameStyle = easyxf(
    'font: name Arial, colour dark_blue, bold true;'
    #'borders: left thick, right thick, top thick, bottom thick;'
    #'pattern: pattern solid, fore_colour red;'
    'alignment: horizontal center;'
)

headerRuntimeOddStyle = easyxf(
    'font: name Arial, colour black, bold true;'
    #'borders: left thick, right thick, top thick, bottom thick;'
    'pattern: pattern solid, fore_colour sky_blue;'
    'alignment: vertical center, horizontal center, rotation 90;'
)

headerRuntimeEvenStyle = easyxf(
    'font: name Arial, colour black, bold true;'
    #'borders: left thick, right thick, top thick, bottom thick;'
    'pattern: pattern solid, fore_colour light_blue;'
    'alignment: vertical center, horizontal center, rotation 90;'
)

headerSystemOddStyle = easyxf(
    'font: name Arial, colour black, bold true;'
    #'borders: left thick, right thick, top thick, bottom thick;'
    'pattern: pattern solid, fore_colour ice_blue;'
    'alignment: vertical center, horizontal center, rotation 90;'
)

separatorStyle = easyxf(
    #'borders: left thick, right thick, top thick, bottom thick;'
    'pattern: pattern fine_dots, fore_colour light_orange;'
)

missingNumberStyle = easyxf (
    'pattern: pattern solid, fore_colour red;'
)

# this is some configuration for where to 
# start printing headers and so forth
hdr_r_start = 0
hdr_y_width = 3
app_data_pts = 2

sequentialTimes = {}
delite1Times = {}
matlab1Times = {}


def main():
    usage = "usage: %prog [options] filename.xls"
    parser = OptionParser(usage)
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose")
    parser.add_option("-t", "--times.dir", default="_default", dest="times_dir", help="directory with times folders")
    parser.add_option("-o", "--output.dir", default="_default", dest="output_dir", help="excel file output directory")
    parser.add_option("-d", "--discard", default=5, type="int", dest="discard", help="how many data points to discard from times folder")
    (options, args) = parser.parse_args()
    
    if len(args) !=1:
        parser.error("incorrect number of arguments")
    xlFile = args[0]
    
    processOptions(options)
        
	# load time files
    # list timesFolder and the datetimes
    files = os.listdir(timesDir)
    timesFolders = deque()
    for fname in files:
        # check if it is a directory
        if os.path.isdir(timesDir + fname) == True:
            if options.verbose:
                print "Found times directory: " + fname
            timesFolders.append(fname)
            
    # create excel sheet
    book = Workbook()
    dataSheet = book.add_sheet('Data')
    #dataSheet.col(0).width = 3000
    rowIdx = printExcelHeader(dataSheet, apps)
    
    # calculate statistics
    # for now, pick latest folder, which should be last on the list
    if options.verbose:
        print "***************************************\n**    Processing Time Folders\n***************************************"
    timesFolderDate = timesFolders.pop()
    if options.verbose: 
        print "Processing Times Directory: " + timesFolderDate + "\n======================================="
    # process times for each apps in this folder
    
    #TFLOP section
    dataSheet.write_merge(rowIdx, rowIdx + len(delite1_procs) + len(delite2_procs) + len(matlab1_procs) + 5, hdr_y_width - 3, hdr_y_width - 3, "TFLOP", headerSystemOddStyle)
    
    #Delite 2.0 section
    dataSheet.write_merge(rowIdx, rowIdx + len(delite2_procs), hdr_y_width - 2, hdr_y_width - 2, "Delite 2.0", headerRuntimeOddStyle)
    appIdx = 0
    for app in apps:
        # load application 
        if options.verbose:
            print "Loading times for application: " + app
        procIdx = 0
        for proc in delite2_procs:
            app_row = rowIdx + procIdx
            # put num threads in column
            if appIdx == 0:
                dataSheet.row(app_row).write(hdr_y_width - 1, str(proc) + " CPU")
            filename = timesDir + timesFolderDate + "/" + app + "-smp-" + str(proc) + ".times"
            app_col = hdr_y_width + appIdx * app_data_pts
            if os.path.isfile(filename):
                appFile = open(filename, 'r')
                numbers = appFile.readlines()
                appFile.close()
                # convert to times
                times = [float(t) for t in numbers]
                times = times[options.discard:len(numbers)]
                mean = numpy.mean(times)
                min = numpy.amin(times)
                max = numpy.amax(times)
                if proc == 1:
                    sequentialTimes[app] = mean                
                dataSheet.row(app_row).write(app_col, mean)
                dataSheet.row(app_row).write(app_col + 1, sequentialTimes[app]/mean)
            else:
                dataSheet.row(app_row).write(app_col, "", missingNumberStyle)
                dataSheet.row(app_row).write(app_col + 1, "", missingNumberStyle)
            procIdx += 1
        if appIdx == 0:
            dataSheet.row(rowIdx + len(delite2_procs)).write(hdr_y_width - 1, "GPU")
        filename = timesDir + timesFolderDate + "/" + app + "-gpu" + ".times"
        if os.path.isfile(filename):
            appFile = open(filename, 'r')
            numbers = appFile.readlines()
            appFile.close()
            # convert to times
            times = [float(t) for t in numbers]
            times = times[options.discard:len(numbers)]
            mean = numpy.mean(times)
            min = numpy.amin(times)
            max = numpy.amax(times)
            dataSheet.row(app_row+1).write(app_col, mean)
            dataSheet.row(app_row+1).write(app_col + 1, sequentialTimes[app]/mean)        
        else:
            dataSheet.row(app_row+1).write(app_col, "", missingNumberStyle)
            dataSheet.row(app_row+1).write(app_col + 1, "", missingNumberStyle)                
        appIdx += 1
    rowIdx += len(delite2_procs) + 1
        
    #load the times
    filename = timesDir + "delite1.times"
    if os.path.isfile(filename):
        #Separator
        dataSheet.write_merge(rowIdx, rowIdx, hdr_y_width - 2, hdr_y_width + len(apps)*2 - 1, "", separatorStyle)
        rowIdx += 1
        
        #Delite 1.0 section
        dataSheet.write_merge(rowIdx, rowIdx + len(delite1_procs), hdr_y_width - 2, hdr_y_width - 2, "Delite 1.0", headerRuntimeEvenStyle)
        
        if options.verbose:
            print "**************************************\nGenerating Delite 1.0 Numbers\n******************************"
        f = open(filename , 'r')
        for line in f:
            tokens = line.split(' ')
            app = tokens.pop(0)
            delite1Times[app] = {}
            #gpu cases
            if len(tokens) == 1:
                delite1Times[app][1] = float(tokens.pop(0))
            else:
                for p in delite1_procs:
                    time = tokens.pop(0)
                    delite1Times[app][p] = float(time)            
        f.close()
        appIdx = 0
        for app in apps:
            # load application 
            if options.verbose:
                print "Loading times for application: " + app
            procIdx = 0
            for proc in delite1_procs:
                app_row = rowIdx + procIdx
                # put num threads in column
                if appIdx == 0:
                    dataSheet.row(app_row).write(hdr_y_width - 1, str(proc) + " CPU")        
                app_col = hdr_y_width + appIdx * app_data_pts            
                dataSheet.row(app_row).write(app_col, delite1Times[app][proc])
                dataSheet.row(app_row).write(app_col + 1, sequentialTimes[app]/delite1Times[app][proc])
                procIdx += 1
            if appIdx == 0:
                dataSheet.row(app_row + 1).write(hdr_y_width - 1, "GPU") 
            dataSheet.row(app_row + 1).write(app_col, delite1Times[app + '-gpu'][1])
            dataSheet.row(app_row + 1).write(app_col + 1, sequentialTimes[app]/delite1Times[app + '-gpu'][1])
            appIdx += 1
        rowIdx += len(delite1_procs) + 1
    
    filename = timesDir + "matlab1.times"
    if os.path.isfile(filename):
        #Separator
        dataSheet.write_merge(rowIdx, rowIdx, hdr_y_width - 2, hdr_y_width + len(apps)*2 - 1, "", separatorStyle)
        rowIdx += 1
        
        # Matlab Section
        dataSheet.write_merge(rowIdx, rowIdx + len(delite1_procs) +1, hdr_y_width - 2, hdr_y_width - 2, "Matlab", headerRuntimeOddStyle)
               
        if options.verbose:
            print "**************************************\nGenerating Matlab Numbers\n******************************"
        #load the times
        f = open(filename, 'r')
        for line in f:
            tokens = line.split(' ')
            app = tokens.pop(0)
            matlab1Times[app] = {}
            #gpu cases
            if len(tokens) == 1:
                matlab1Times[app][1] = float(tokens.pop(0))
            else:
                for p in matlab1_procs:
                    time = tokens.pop(0)
                    matlab1Times[app][p] = float(time)            
        f.close()
        appIdx = 0
        for app in apps:
            # load application 
            if options.verbose:
                print "Loading times for application: " + app
            procIdx = 0
            for proc in matlab1_procs:
                app_row = rowIdx + procIdx
                # put num threads in column
                if appIdx == 0:
                    dataSheet.row(app_row).write(hdr_y_width - 1, str(proc) + " CPU")        
                app_col = hdr_y_width + appIdx * app_data_pts                            
                dataSheet.row(app_row).write(app_col, matlab1Times[app][proc])
                if sequentialTimes.has_key(app):
                    dataSheet.row(app_row).write(app_col + 1, sequentialTimes[app]/matlab1Times[app][proc])
                else:
                    dataSheet.row(app_row).write(app_col + 1, "", missingNumberStyle)
                procIdx += 1
            if appIdx == 0:
                dataSheet.row(app_row + 1).write(hdr_y_width - 1, "GPU") 
                dataSheet.row(app_row + 2).write(hdr_y_width - 1, "Jacket") 
            dataSheet.row(app_row + 1).write(app_col, matlab1Times[app + '-gpu'][1])
            if sequentialTimes.has_key(app):
                dataSheet.row(app_row + 1).write(app_col + 1, sequentialTimes[app]/matlab1Times[app + '-gpu'][1])
            else:
                dataSheet.row(app_row + 1).write(app_col + 1, "", missingNumberStyle)
            dataSheet.row(app_row + 2).write(app_col, matlab1Times[app + '-jacket'][1])
            if sequentialTimes.has_key(app):
                dataSheet.row(app_row + 2).write(app_col + 1, sequentialTimes[app]/matlab1Times[app + '-jacket'][1])
            else:
                dataSheet.row(app_row + 2).write(app_col + 1, "", missingNumberStyle)
            appIdx += 1
        rowIdx += len(matlab1_procs) + 2
            
    if options.verbose:
        print "**************************************\nGenerating dashboard excel spreadsheet to %s..." % xlFile    
    book.save(outputDir + xlFile)
    
def processOptions(options):
    global timesDir
    global outputDir
    
    if(options.times_dir != "_default"):
        timesDir = options.times_dir
    if(options.output_dir != "_default"):
        outputDir = optines.output_dir
    

##
# prints the Dashboard Excel Header
# returns the row at which the rest of the table should continue
def printExcelHeader(sheet, apps):
    #print top level header
    #merge cell
    merge_length = max(len(apps)*app_data_pts, 5)
    sheet.write_merge(hdr_r_start,hdr_r_start,hdr_y_width,hdr_y_width+merge_length, 'Execution time by application [sec]', easyxf(
        'font: name Arial, colour dark_blue, bold true, height 250;'
        #'borders: left thick, right thick, top thick, bottom thick;'
        #'pattern: pattern solid, fore_colour red;'
        'alignment: horizontal center;'
    ))
    #print application headers
    appIdx=0
    for app in apps:
        r1 = hdr_r_start+1
        r2 = r1
        c1 = hdr_y_width+appIdx*app_data_pts
        c2 = c1 + app_data_pts - 1
        sheet.write_merge(r1, r2, c1, c2, app, headerAppNameStyle)          
        sheet.row(r1+1).set_cell_text(c1, 'time', labelStyle)
        sheet.row(r1+1).set_cell_text(c1+1, 'speedup', labelStyle)        
        appIdx +=1
    #print table header
    row = sheet.row(hdr_r_start+2)
    row.write(0, 'System', labelStyle)
    row.write(1, 'Runtime', labelStyle)
    row.write(2, 'Threads', labelStyle)
    return 3
    
    

    
if __name__ == "__main__":
    main()
    
