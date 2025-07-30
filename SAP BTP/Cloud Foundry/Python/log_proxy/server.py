import os, http.server, http.client
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import urlparse

parsed_url = urlparse(os.getenv('destination_url'))
bearer_token = os.getenv('bearer_token')

class SimpleHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        post_data = self.rfile.read(int(self.headers['Content-Length']))
        conn = http.client.HTTPSConnection(parsed_url.netloc)
        headers = {
            'Content-Type': self.headers['Content-Type'],
            'Content-Encoding': self.headers['Content-Encoding'] or b'',
            'Authorization': 'Bearer ' + bearer_token
        }
        conn.request("POST", parsed_url.path, body=post_data, headers=headers)
        self.send_response(conn.getresponse().status)
        self.send_header("Content-type", "text/plain")
        self.end_headers()
        conn.close()

# Create and start the server
HTTPServer(('', int(os.getenv('PORT', 8080))), SimpleHandler).serve_forever()
