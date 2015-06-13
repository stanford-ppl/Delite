#!/usr/bin/env python
 
import json
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from urlparse import urlparse, parse_qs
from os import path
import sqlite3
import sys

class Object:
	pass

config = Object()

def init(dir):
	config.dslDir 		= path.abspath( dir )
	config.profileDir 	= path.join( config.dslDir, "profile" )
	config.dbFile 		= path.join( config.profileDir, "profile.db" )
	config.gcStatsFile 	= path.join( config.profileDir, "gcStats.txt" )
	config.uiDataFile	= path.join( config.profileDir, "profileDataUI.js" )
	config.appSrcDir	= path.join( config.dslDir, "apps/src" )

	config.dbConn 			  = sqlite3.connect(config.dbFile)
	config.dbConn.row_factory = sqlite3.Row
	config.dbCursor 		  = config.dbConn.cursor()

class MyServer(BaseHTTPRequestHandler):
	def send_headers(self, contentType):
		self.send_response( 200 )
	 	self.send_header( "Access-Control-Allow-Origin", "*" )
	 	self.send_header( 'Content-type', contentType )
	 	self.end_headers()

	def send_file(self, filePath):
		f = open( filePath )
		self.wfile.write(f.read())
	 	f.close()

	def send_gc_stats(self):
		self.send_headers( 'text/plain' )
		self.send_file( config.gcStatsFile )

	def send_ui_data(self):
		self.send_headers( 'application/json' )
		self.send_file( config.uiDataFile )

	def send_app_src(self, fileName):
		self.send_headers( 'text/plain' )
		filePath = path.join( config.appSrcDir, fileName)
		if path.isfile( filePath ):
			self.send_file( filePath )
		else:
			print "[ERROR]: Could not find source file: ", filePath

	def handle_db_query(self, query):
		self.send_headers( 'application/json' )
		result = self.fetchDataFromDB( query )
		self.wfile.write( result )

	def fetchDataFromDB(self, query):
		config.dbCursor.execute( query )
		rows = config.dbCursor.fetchall()
		return json.dumps( map( lambda row: self.rowToJSON( row ), rows ) )

	def rowToJSON(self, row):
		return dict( ( config.dbCursor.description[i][0], value ) for i, value in enumerate( row ) )

	def do_GET(self):
		print "Request received: ", self.path		
		params = parse_qs(urlparse(self.path).query)
		
		if 'query' in params:
			query = params['query'][0]
			self.handle_db_query( query )
			return

		req = params['f'][0]
		
		if ( req == 'profileDataUI' ):
			self.send_ui_data()
		elif ( req == 'gcStats' ):
			self.send_gc_stats()
		elif ( req == 'appSourceFile' ):
			fileName = params['name'][0]
			self.send_app_src( fileName )

	 	print "Request serviced."
	  
def run():
  server_address = ( '127.0.0.1', 8000 )
  dslRootDir = path.realpath( sys.argv[1] )
  init( dslRootDir )

  print "Starting server"
  httpd = HTTPServer(server_address, MyServer)
  httpd.serve_forever()
  
if __name__ == '__main__':
  run()