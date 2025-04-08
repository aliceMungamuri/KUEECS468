/*

  
  - GET: Read files and directories
  - PUT: Write files
  - DELETE: Delete files and directories
  - MKCOL: Create directories
  Inputs:
  - HTTP requests with method and path
  Output:
  - Response with appropriate status code and message or file contents
  Collaborators: None
  Other sources: ChatGPT, Stack Overflow, Node.js docs (fs, http, url)
   Alice J Mungamuri
   April 7, 2025
*/

const http = require("http"); // import the http module so I can create a server
const fs = require("fs"); // inport the fs module with the promise based api
const { parse } = require("url"); // imported url module to get the path name the URL
const path = require("path"); // imports the path module for the file paths

const baseDirectory = path.resolve("./storage"); // this is the root directory - not necessary stack overflow


function sendResponse(res, status, message) {// Function to send response
  res.writeHead(status, { "Content-Type": "text/plain" }); // send a plain text response with a given HTTP status code and message
  res.end(message); // endedd
}


function getSafePath(urlPath) {// Function to get safe path
  const resolvedPath = path.resolve(baseDirectory + urlPath); //mixes the base directory with the request path
  if (!resolvedPath.startsWith(baseDirectory)) { //If the final path attempts to go outside the base directory
    throw { code: 403 }; //throws a 403 error.
  }
  return resolvedPath; // normailizes it chat gpt
}


const server = http.createServer((req, res) => {// Create HTTP server
  const method = req.method; // gets the method
  const urlPath = parse(req.url).pathname; // request url and takes the pathname

  let filePath;// let file path
  try { // try block
    filePath = getSafePath(urlPath);// to get a safe version of the file path
  } catch (err) { // invalid
    return sendResponse(res, 403, "403 Forbidden"); // then reply 403 forbidden
  }

  if (method === "GET") {  // Handle GET requests

    fs.stat(filePath, (err, stats) => { // path exists?
      if (err) return sendResponse(res, 404, "404 File Not Found"); // if it doesnt 404
      if (stats.isDirectory()) { // if it is a directory 
        fs.readdir(filePath, (err, files) => { // read contents
          if (err) return sendResponse(res, 500, "500 Internal Server Error"); // otherwise 500 Error
          res.writeHead(200, { "Content-Type": "text/plain" });// responds list of files
          res.end(files.join("\n")); // end
        });
      } else {
        fs.readFile(filePath, (err, data) => {
          if (err) return sendResponse(res, 500, "500 Internal Server Error");
          res.writeHead(200, { "Content-Type": "text/plain" });
          res.end(data);
        });
      }
    });

  } else if (method === "PUT") {  // Handle PUT requests
    const writeStream = fs.createWriteStream(filePath); //writable stream to the file path.
    req.pipe(writeStream); //Pipes the incoming request body (file content) into the file
    req.on("end", () => sendResponse(res, 200, "File written successfully.")); // if written success
    req.on("error", () => sendResponse(res, 500, "500 Internal Server Error")); // if 500 error

  } else if (method === "DELETE") {  // Handle DELETE requests
    fs.stat(filePath, (err, stats) => { // if the file or directory exists 
      if (err) return sendResponse(res, 404, "404 File Not Found"); // respond that if not found
      if (stats.isDirectory()) { //f it's a director
        fs.rmdir(filePath, { recursive: true }, (err) => { // remove it recursively
          if (err) return sendResponse(res, 500, "500 Internal Server Error"); // respond 500 error
          sendResponse(res, 200, "Directory deleted."); // succesful responsee
        });
      } else {
        fs.unlink(filePath, (err) => {//If it's a file, 
          if (err) return sendResponse(res, 500, "500 Internal Server Error"); // 500 error
          sendResponse(res, 200, "File deleted."); //delete it using fs.unlink
        });
      }
    });

  } else if (method === "MKCOL") {  // Handle MKCOL (make collection / create directory)
    fs.mkdir(filePath, { recursive: false }, (err) => { // create directories
      if (err) { //If the folder exist
        if (err.code === 'EEXIST') return sendResponse(res, 400, "400 Not a Directory"); //return a 400 error
        return sendResponse(res, 500, "500 Internal Server Error");
      }
      sendResponse(res, 201, "Directory created."); // else  return a 201 Created
    });

  } else {  // Handle unsupported methods
    sendResponse(res, 405, `The method ${method} is not supported.`); // respond with 405 Method Not Allowed
  }
});

server.listen(8000, () => { //tarts the HTTP server and listens for  theconnections on port 8000
  console.log("Server running on http://localhost:8000");// message once the server is up
});
