apps = ['gda', 'nb', 'linreg', 'kmeans', 'rbm']#, 'lbp','svm']
procs = [ 1, 2, 4, 8, 16]
dataPath = "C:/Users/hchafi/Documents/Dropbox/ppl/ppl/delite/data/"
outputFolder = "C:/Users/hchafi/Documents/Dropbox/ppl/ppl/delite/data/"


import os

from optparse import OptionParser
from xlwt import Workbook, easyxf
from collections import deque
import numpy


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


# this is some configuration for where to 
# start printing headers and so forth
hdr_r_start = 0
hdr_y_width = 3
app_data_pts = 2

sequentialTimes = {}
delite1Times = {}

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

def main():
    usage = "usage: %prog [options] filename.xls"
    parser = OptionParser(usage)
    parser.add_option("-o", "--overwrite", dest="overwrite", action="store_true", 
                      help="overwrite excel file if it already exists")
    parser.add_option("-u", "--update", dest="update", action="store_true", 
                      help="update excel file if it already exists")
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose")
    (options, args) = parser.parse_args()
    if len(args) !=1:
        parser.error("incorrect number of arguments")
    xlFile = args[0]
    
	# check the required env variables
	#if DATA_DIR is None:
	#	exit("DATA_DIR not defined, needs to be set to point to directory that contains the data folder with datasets")
        
	# load time files
    # list timesFolder and the datetimes
    files = os.listdir(dataPath)
    timesFolders = deque()
    for fname in files:
        # check if it is a directory
        if os.path.isdir(dataPath + fname) == True:
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
    dataSheet.write_merge(rowIdx, rowIdx + len(procs)*2, hdr_y_width - 3, hdr_y_width - 3, "TFLOP", headerSystemOddStyle)
    
    #Delite 2.0 section
    dataSheet.write_merge(rowIdx, rowIdx + len(procs) - 1, hdr_y_width - 2, hdr_y_width - 2, "Delite 2.0", headerRuntimeOddStyle)
    appIdx = 0
    for app in apps:
        # load application 
        if options.verbose:
            print "Loading times for application: " + app
        procIdx = 0
        for proc in procs:
            app_row = rowIdx + procIdx
            # put num threads in column
            if appIdx == 0:
                dataSheet.row(app_row).write(hdr_y_width - 1, str(proc) + " CPU")
            appFile = open(dataPath + timesFolderDate + "/" + app + "-smp-" + str(proc) + ".times", 'r')
            numbers = appFile.readlines()
            appFile.close()
            # convert to times
            times = [float(t) for t in numbers]
            times = times[5:len(numbers)]
            mean = numpy.mean(times)
            min = numpy.amin(times)
            max = numpy.amax(times)
            if proc == 1:
                sequentialTimes[app] = mean
            app_col = hdr_y_width + appIdx * app_data_pts
            
            dataSheet.row(app_row).write(app_col, mean)
            dataSheet.row(app_row).write(app_col + 1, sequentialTimes[app]/mean)
            procIdx += 1
        appIdx += 1
    rowIdx += len(procs)
    
    #Separator
    dataSheet.write_merge(rowIdx, rowIdx, hdr_y_width - 2, hdr_y_width + len(apps)*2 - 1, "", separatorStyle)
    rowIdx += 1
    
    #Delite 1.0 section
    dataSheet.write_merge(rowIdx, rowIdx + len(procs) - 1, hdr_y_width - 2, hdr_y_width - 2, "Delite 1.0", headerRuntimeEvenStyle)
    
    if options.verbose:
        print "**************************************\nGenerating Delite 1.0 Numbers\n******************************"
    #load the times
    f = open(dataPath +  "delite1.times", 'r')
    for line in f:
        tokens = line.split(' ')
        app = tokens.pop(0)
        delite1Times[app] = {}  
        for p in procs:
            time = tokens.pop(0)
            delite1Times[app][p] = float(time)            
    f.close()
    appIdx = 0
    for app in apps:
        # load application 
        if options.verbose:
            print "Loading times for application: " + app
        procIdx = 0
        for proc in procs:
            app_row = rowIdx + procIdx
            # put num threads in column
            if appIdx == 0:
                dataSheet.row(app_row).write(hdr_y_width - 1, str(proc) + " CPU")        
            app_col = hdr_y_width + appIdx * app_data_pts            
            dataSheet.row(app_row).write(app_col, delite1Times[app][proc])
            dataSheet.row(app_row).write(app_col + 1, delite1Times[app][1]/delite1Times[app][proc])
            procIdx += 1
        appIdx += 1
    rowIdx += len(procs)
        
    if options.verbose:
        print "**************************************\nGenerating dashboard excel spreadsheet to %s..." % xlFile    
    book.save(outputFolder + xlFile)
    
    

    
if __name__ == "__main__":
    main()
    
