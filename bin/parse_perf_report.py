#!/usr/bin/python

import sqlite3
import subprocess
import sys

IDX_SAMPLES = 1
IDX_MEM_ACCESS = 2
IDX_DATA_TARGET_ADDR = 3

class DataStructure:
	def __init__( self, startAddr, size, sourceContext = "SCNotProvided" ):
		self.startAddr = int(startAddr, 16)
		self.endAddr = self.startAddr + size
		self.sourceContext = sourceContext

class MemAccessStats:
	def __init__( self ):
		self.l1Hits = 0
		self.l2Hits = 0
		self.l3Hits = 0
		self.localRAMHits = 0
		self.remoteRAMHits = 0

def hexToInt( hexStr ):
	try:
		return int( hexStr, 16 )
	except ValueError:
		return None

def findDataStructureAccessed( dataStructures, targetDataAddr ):
	if targetDataAddr != None:
		for ds in dataStructures:
			if ( ds.startAddr <= targetDataAddr ) and ( targetDataAddr <= ds.endAddr ):
				return ds

	return None

def updateMemAccessStats( dataStructureToStats, dataStructureAccessed, memAccessStr, samplesCount ):
	if not dataStructureAccessed in dataStructureToStats:
		dataStructureToStats[dataStructureAccessed] = MemAccessStats()

	stats = dataStructureToStats[dataStructureAccessed]
	
	if memAccessStr == "L1 hit":
		stats.l1Hits = stats.l1Hits + samplesCount
	elif memAccessStr == "L2 hit":
		stats.l2Hits = stats.l2Hits + samplesCount
	elif memAccessStr == "L3 hit":
		stats.l3Hits = stats.l3Hits + samplesCount
	elif memAccessStr == "Local RAM hit":
		stats.localRAMHits = stats.localRAMHits + samplesCount
	elif memAccessStr.startswith("Remote RAM"):
		stats.remoteRAMHits = stats.remoteRAMHits + samplesCount

def dumpStats( dataStructureToStats ):
	dbFile = '/home/jithinpt/cache_instrumentation/hyperdsl/published/SimpleVector/profile/profile.db'
	conn = sqlite3.connect( dbFile )
	c = conn.cursor()

	for ds in dataStructureToStats:
		stats = dataStructureToStats[ds]
		tot = stats.l1Hits + stats.l2Hits + stats.l3Hits + stats.localRAMHits + stats.remoteRAMHits
		l1HitPct = int( (stats.l1Hits * 100) / tot )
		l2HitPct = int( (stats.l2Hits * 100) / tot )
		l3HitPct = int( (stats.l3Hits * 100) / tot )
		localRAMHitPct = int( (stats.localRAMHits * 100) / tot )
		remoteRAMHitPct = int( (stats.remoteRAMHits * 100) / tot )
	
		sql = 'INSERT INTO ArrayCacheAccessStats VALUES ({0},{1},{2},{3},{4},{5});\n'.format(ds.sourceContext, l1HitPct, l2HitPct, l3HitPct, localRAMHitPct, remoteRAMHitPct)
		c.execute(sql)
		conn.commit()

	conn.close()

def parsePerfReport( report, dataStructures ):
	dataStructureToStats = {}
	isCommentsSection = True

	perfReportCmd = 'perf report --mem-mode -n --sort=mem,symbol_daddr -t , -i ' + report
	proc = subprocess.Popen( perfReportCmd.split(' '), stdout = subprocess.PIPE )

	while True:
		line = proc.stdout.readline()
		if not line: break

		line = line.rstrip().strip('\n')

		if line == "": continue

		if line[0] != "#":
			isCommentsSection = False

		if not isCommentsSection:
			arr = line.split(",")
			samplesCount = int( arr[IDX_SAMPLES].strip() )
			memAccessStr = arr[IDX_MEM_ACCESS].strip()
			targetDataAddr = hexToInt( arr[IDX_DATA_TARGET_ADDR].strip().split()[1] )
			
			dataStructureAccessed = findDataStructureAccessed( dataStructures, targetDataAddr )	
			if dataStructureAccessed != None:
				updateMemAccessStats( dataStructureToStats, dataStructureAccessed, memAccessStr, samplesCount )
	dumpStats( dataStructureToStats )

def parseDataStructureInfo( dsFile ):
	dataStructures = []
	f = open( dsFile, 'r' )
	for line in f:
		arr = line.strip('\n').split(",")
		startAddr = arr[0]
		size = int(arr[1])
		sourceContext = arr[2]
		dataStructures.append( DataStructure(startAddr, size, sourceContext) )
	
	f.close()
	return dataStructures
 
def main():
	print("Parsing perf data")
	perfDataFile = '/home/jithinpt/cache_instrumentation/hyperdsl/published/SimpleVector/perf.data' 
	dataStructureMemRangesFile = '/home/jithinpt/cache_instrumentation/hyperdsl/published/SimpleVector/dataStructures.csv'
	dataStructures = parseDataStructureInfo( dataStructureMemRangesFile )
	parsePerfReport( perfDataFile, dataStructures )

if __name__ == "__main__":
    main()
