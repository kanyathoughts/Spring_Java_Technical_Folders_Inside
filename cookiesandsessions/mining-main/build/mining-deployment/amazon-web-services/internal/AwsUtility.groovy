@Grab('software.amazon.awssdk:http-client-spi:2.16.57')

import software.amazon.awssdk.utils.AttributeMap
import software.amazon.awssdk.http.SdkHttpConfigurationOption
import software.amazon.awssdk.http.apache.ApacheHttpClient

/**
 * Creates an ApacheHttpClient that is used to disable ssl validation.
 * @return ApacheHttpClient object
 */
private ApacheHttpClient trustAllCertificates() {
    try {
        AttributeMap configMap = AttributeMap.builder().put(SdkHttpConfigurationOption.TRUST_ALL_CERTIFICATES, true).build()
        /* we set the option TRUST_ALL_CERTIFICATES for the AttributeMap and pass this to the ApacheHttpClient builder */
        return ApacheHttpClient.builder().buildWithDefaults(configMap)
    } catch (Exception e) {
        e.printStackTrace()
        return null
    }
}
