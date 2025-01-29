package com.example.poctest123;

import org.apache.tomcat.util.http.fileupload.FileUtils;
import org.apache.tomcat.util.http.fileupload.IOUtils;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

@RestController
public class LangController {

    //Can be added to the application.yml or another more appropriate implementation can be taken for these constants
    private static String pythonLocation = "/Library/Frameworks/Python.framework/Versions/3.11/bin/python3";
    private static final String singlePyLocation = "singleFile.py";

    private static final String topTenLocation = "topTen.py";
    private static final String zipClassLocation = "zipClass.py";

    private final GuessLangConfig config = new GuessLangConfig();

    /**
     * Returns the most likely language type for the input file
     *
     * @param file the input source code to be classified
     * @return a String containing the most likely language type
     */
    @PostMapping("/getguess")
    public String uploader(@RequestParam("file")MultipartFile file) throws IOException{
        if(file.isEmpty()){
            return "NO FILE, PLEASE FIX";
        }
        else {
            StringBuilder content = new StringBuilder();
            try(BufferedReader reader = new BufferedReader(new InputStreamReader(file.getInputStream()))) {
                int charCode;
                while ((charCode = reader.read()) != -1) {
                    char character = (char) charCode;
                    content.append(character);
                }
            }
            try {
                String files = content.toString();

                //TODO Change the location of the Python Installation location when Deployed
                ProcessBuilder processBuilder = new ProcessBuilder(Arrays.asList(pythonLocation, singlePyLocation));

                Process process = processBuilder.start();

                BufferedReader br = new BufferedReader(new InputStreamReader(process.getInputStream()));
                String line;

                OutputStream os = process.getOutputStream();
                PrintWriter pw = new PrintWriter(os);
                String[] fileDataLines = files.split("%n");
                for (String dataLine : fileDataLines) {
                    pw.write(dataLine);
                }
                pw.flush();
                pw.close();
                StringBuilder output = new StringBuilder();

                while ((line = br.readLine()) != null) {
                    output.append(line);
                }
                return  output.toString();

            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return "";
    }

    /**
     * Sets the Python location for the process builder to whatever the input is
     *
     * @param pyLoc the location of python wherever this application is deployed
     * @return a String with a confirmation message
     */
    @PostMapping("/setpython")
    public String setLocations(@RequestBody String pyLoc) {
        pythonLocation = pyLoc;
        return "Python location set to " +pyLoc;

    }
    /**
     * Sets the Confidence Interval for GuessLang to classify something as maybe or as the highests probability file type
     *
     * @param confid a String that is a number of the new confide interval from 0-100
     * @return a String with a confirmation message or failure message if
     */
    @PostMapping("/setconfidinterval")
    public String setConfid(@RequestBody String confid) {
        try {
            config.setConfidenceInterval(Integer.parseInt(confid));
            return  "Set new confidence interval to " + confid;
        }
        catch (ClassCastException e) {
            return "Failed as input is not a number";
        }
    }


    /**
     * Returns the top ten most likely languages for the input file
     *
     * @param file The input source code to be classified
     * @return a String containing the top 10 most likely language types (can be parsed as json)
     */
    @PostMapping("/gettopten")
    public String setLocations(@RequestParam("file")MultipartFile file) throws IOException {
        if(file.isEmpty()){
            return "NO FILE, PLEASE FIX";
        }
        else {
            StringBuilder content = new StringBuilder();
            try(BufferedReader reader = new BufferedReader(new InputStreamReader(file.getInputStream()))) {
                int charCode;
                while ((charCode = reader.read()) != -1) {
                    char character = (char) charCode;
                    content.append(character);
                }
            }
            try {
                String files = content.toString();

                //TODO Change the location of the Python Installation location when Deployed
                ProcessBuilder processBuilder = new ProcessBuilder(Arrays.asList(pythonLocation, topTenLocation));
                Process process = processBuilder.start();

                BufferedReader br = new BufferedReader(new InputStreamReader(process.getInputStream()));
                StringBuilder pyOutput = new StringBuilder();

                OutputStream os = process.getOutputStream();
                PrintWriter pw = new PrintWriter(os);
                String[] fileDataLines = files.split("%n");
                for (String dataLine : fileDataLines) {
                    pw.write(dataLine);
                }
                pw.flush();
                pw.close();

                String line = "";
                while ((line = br.readLine()) != null) {
                    pyOutput.append(line);
                }
                String pyString = pyOutput.toString();

                int exit = process.waitFor();
                if(exit == 0) {
                    return pyString;
                }
                else{
                    return "Error in the Pythron";
                }

            } catch (IOException e) {
                e.printStackTrace();
            } catch (InterruptedException e) {
				throw new RuntimeException(e);
			}

		}
        return null;
    }




    /**
     * Returns a zip containing directories corresponding to file type names containing the input files with extensions appied
     *
     * @param zip a zip containing the files that need to be classified
     * @return a zip containing the directories corresponding to the identified languages with each directory containing
     * files corresponding to it with the extensions applied. Files
     */
    @RequestMapping(value = "/zipsorter", produces="application/zip")
    public ResponseEntity<FileSystemResource> zipper (@RequestParam("file") MultipartFile zip) throws IOException {
        try{

            FileUtils.cleanDirectory(new File("sorted"));

            if(!("application/zip".equals(zip.getContentType()))) {
                System.out.println("not a zip");
            }
            ZipInputStream inputStream = new ZipInputStream(zip.getInputStream());
            ZipEntry entry;

            while ((entry = inputStream.getNextEntry())!=null) {
                if (!(entry.isDirectory())){
                    String fileName = entry.getName();
                    BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
                    String line ;
                    StringBuilder input = new StringBuilder();
                    int charCode;
                    while ((charCode = reader.read()) != -1) {
                        char character = (char) charCode;
                        input.append(character);
                    }
                    ProcessBuilder processBuilder = new ProcessBuilder(pythonLocation, zipClassLocation, "20");
                    Process process = processBuilder.start();
                    String codeAsString = input.toString();
                    OutputStream os = process.getOutputStream();
                    PrintWriter pw = new PrintWriter(os);
                    String[] fileDataLines = codeAsString.split("\n");
                    for (String dataLine : fileDataLines) {
                        pw.write(dataLine);
                    }
                    pw.flush();
                    pw.close();
                    BufferedReader br = new BufferedReader(new InputStreamReader(process.getInputStream()));
                    StringBuilder output = new StringBuilder();

                    while ((line = br.readLine()) != null) {
                        output.append(line);
                    }
                    String res = output.toString();
                    String fileType = "";
                    String extension = "";
                    if(res.contains("maybe")){
                        fileType =  res.substring((res.indexOf("%")+1));
                        extension = config.getFileExtension(fileType);
                        fileType = "maybe";

                    }
                    else {
                         fileType = res;
                         extension =config.getFileExtension(res);
                    }

                    //append to fileName and sort
                    String updatedFileName = fileName + extension;
                    String dirToPlaceFile = "sorted/"+ fileType;
                    File sortedDir = new File(dirToPlaceFile);
                    if (!(sortedDir.exists())) {
                        boolean created = sortedDir.mkdirs();
                        if (!created) {
                            System.out.println("error in dir");
                        }
                    }

                    File resultFile = new File(dirToPlaceFile+"/"+updatedFileName);
                    if(resultFile.createNewFile()) {
                        try (BufferedWriter writer2 = new BufferedWriter(new FileWriter(resultFile))) {
                            writer2.write(codeAsString);
                            writer2.flush();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }


                    }
                    else {
                        System.out.println("failure in writing contents to file");
                    }

                }

            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        File sourceDirectory = new File("sorted");

        if (!sourceDirectory.exists() || !sourceDirectory.isDirectory()) {
            return ResponseEntity.notFound().build();
        }

        File zipFile = File.createTempFile("directory", ".zip");

        try (FileOutputStream fos = new FileOutputStream(zipFile);
                ZipOutputStream zipOut = new ZipOutputStream(fos)) {
            addFilesToZip(sourceDirectory, sourceDirectory, zipOut);
        }

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_OCTET_STREAM);
        headers.setContentDispositionFormData("attachment", zipFile.getName());

        FileSystemResource fileResource = new FileSystemResource(zipFile);
        return new ResponseEntity<>(fileResource, headers, org.springframework.http.HttpStatus.OK);


    }

    private void addFilesToZip(File sourceDirectory, File file, ZipOutputStream zipOut) throws IOException {
        if (file.isDirectory()) {
            File[] files = file.listFiles();
            if (files != null) {
                for (File child : files) {
                    addFilesToZip(sourceDirectory, child, zipOut);
                }
            }
        } else {
            String entryName = sourceDirectory.toURI().relativize(file.toURI()).getPath();
            ZipEntry zipEntry = new ZipEntry(entryName);
            zipOut.putNextEntry(zipEntry);

            try (FileInputStream fis = new FileInputStream(file)) {
                IOUtils.copy(fis, zipOut);
            }
            zipOut.closeEntry();
        }

    }

}
