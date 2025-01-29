package com.springSec.example.springSecurityDemo.Exception;

public class MyCustomException extends Exception {

    private int httpStatusCode;
    private String errorMessage;

    public int getHttpStatusCode() {
        return httpStatusCode;
    }

    public void setHttpStatusCode(int httpStatusCode) {
        this.httpStatusCode = httpStatusCode;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    @Override
    public String toString() {
        return "MyCustomException [httpStatusCode=" + httpStatusCode + ", errorMessage=" + errorMessage + "]";
    }

    public MyCustomException(int httpStatusCode, String errorMessage) {
        this.httpStatusCode = httpStatusCode;
        this.errorMessage = errorMessage;
    }

}
