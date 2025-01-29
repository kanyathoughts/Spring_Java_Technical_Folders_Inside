package com.springSec.example.springSecurityDemo.service;

import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.Base64.Decoder;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;

import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;

@Service
public class JWTService {

    private String secretKey = "";

    // we are generating seceret key when ever object of JWTService class is created
    // We are encoding string into base64 type
    public JWTService() {
        try {
            KeyGenerator keyGen = KeyGenerator.getInstance("HmacSHA256");
            SecretKey sk = keyGen.generateKey();
            secretKey = Base64.getEncoder().encodeToString(sk.getEncoded());
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }

    }

    public String generateToken(String userName) {

        Map<String, Object> claims = new HashMap<>();
        return Jwts.builder()
                // we can set claims with this method
                .claims()
                // we can add our map where we want our claims to be added
                .add(claims)
                // subject is one claim
                .subject(userName)
                // issuedAt is also one claim
                .issuedAt(new Date(System.currentTimeMillis()))
                // expiration is also one claim
                .expiration(new Date(System.currentTimeMillis() + 60 * 60 * 30))
                .and()
                // After adding all of the claims we need to sign the token with one secret key
                .signWith(geyKey())
                // At the end everything will be converted into string
                .compact();
    }

    // We are converting the secret key into Key type
    // We are decoding base64 encoded string and converting into Key type
    private SecretKey geyKey() {
        byte[] keyBytes = Decoders.BASE64.decode(secretKey);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    public String extractUserName(String token) {
        // extract the username from jwt token
        return extractClaim(token, Claims::getSubject);
    }

    private <T> T extractClaim(String token, Function<Claims, T> claimResolver) {
        final Claims claims = extractAllClaims(token);
        return claimResolver.apply(claims);
    }

    private Claims extractAllClaims(String token) {
        return Jwts.parser()
                .verifyWith(geyKey())
                .build()
                .parseSignedClaims(token)
                .getPayload();
    }

    public boolean validateToken(String token, UserDetails userDetails) {
        final String userName = extractUserName(token);
        return (userName.equals(userDetails.getUsername()) && !isTokenExpired(token));
    }

    private boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    private Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

}
