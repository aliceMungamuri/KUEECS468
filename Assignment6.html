/*
  EECS 468 Assignment 6
  HTTP File System Server

  Description:
  This Node.js HTTP server allows clients to interact with a file system remotely using HTTP methods:
  - GET: Read files and directories
  - PUT: Write files
  - DELETE: Delete files and directories
  - MKCOL: Create directories

  Inputs:
  - HTTP requests with method and path

  Output:
  - Response with appropriate status code and message or file contents

  Collaborators: None
  Other sources: ChatGPT, Node.js docs (fs, http, url)
  Author: Alice J Mungamuri
  Creation date: April 7, 2025
*/

const http = require("http"); // Import the HTTP module
const fs = require("fs"); // Import the file system module
const { parse } = require("url"); // Import URL parsing
const path = require("path"); // Import path utilities

const baseDirectory = path.resolve("./storage"); // Base directory for safety

// Function to send response
function sendResponse(res, status, message) {
  res.writeHead(status, { "Content-Type": "text/plain" });
  res.end(message);
}

// Function to get safe path
function getSafePath(urlPath) {
  const resolvedPath = path.resolve(baseDirectory + urlPath);
  if (!resolvedPath.startsWith(baseDirectory)) {
    throw { code: 403 }; // Prevent path traversal
  }
  return resolvedPath;
}

// Create HTTP server
const server = http.createServer((req, res) => {
  const method = req.method;
  const urlPath = parse(req.url).pathname;

  let filePath;
  try {
    filePath = getSafePath(urlPath);
  } catch (err) {
    return sendResponse(res, 403, "403 Forbidden");
  }

  // Handle GET requests
  if (method === "GET") {
    fs.stat(filePath, (err, stats) => {
      if (err) return sendResponse(res, 404, "404 File Not Found");
      if (stats.isDirectory()) {
        fs.readdir(filePath, (err, files) => {
          if (err) return sendResponse(res, 500, "500 Internal Server Error");
          res.writeHead(200, { "Content-Type": "text/plain" });
          res.end(files.join("\n"));
        });
      } else {
        fs.readFile(filePath, (err, data) => {
          if (err) return sendResponse(res, 500, "500 Internal Server Error");
          res.writeHead(200, { "Content-Type": "text/plain" });
          res.end(data);
        });
      }
    });

  // Handle PUT requests
  } else if (method === "PUT") {
    const writeStream = fs.createWriteStream(filePath);
    req.pipe(writeStream);
    req.on("end", () => sendResponse(res, 200, "File written successfully."));
    req.on("error", () => sendResponse(res, 500, "500 Internal Server Error"));

  // Handle DELETE requests
  } else if (method === "DELETE") {
    fs.stat(filePath, (err, stats) => {
      if (err) return sendResponse(res, 404, "404 File Not Found");
      if (stats.isDirectory()) {
        fs.rmdir(filePath, { recursive: true }, (err) => {
          if (err) return sendResponse(res, 500, "500 Internal Server Error");
          sendResponse(res, 200, "Directory deleted.");
        });
      } else {
        fs.unlink(filePath, (err) => {
          if (err) return sendResponse(res, 500, "500 Internal Server Error");
          sendResponse(res, 200, "File deleted.");
        });
      }
    });

  // Handle MKCOL (make collection / create directory)
  } else if (method === "MKCOL") {
    fs.mkdir(filePath, { recursive: false }, (err) => {
      if (err) {
        if (err.code === 'EEXIST') return sendResponse(res, 400, "400 Not a Directory");
        return sendResponse(res, 500, "500 Internal Server Error");
      }
      sendResponse(res, 201, "Directory created.");
    });

  // Handle unsupported methods
  } else {
    sendResponse(res, 405, `The method ${method} is not supported.`);
  }
});

// Listen on port 8000
server.listen(8000, () => {
  console.log("Server running on http://localhost:8000");
});
