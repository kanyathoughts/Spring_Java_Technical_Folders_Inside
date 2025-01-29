/**
 */
package fw2.model2.impl;

import fw2.model2.Action;
import fw2.model2.Model2Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Action</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.ActionImpl#getAccessKey <em>Access Key</em>}</li>
 *   <li>{@link fw2.model2.impl.ActionImpl#getUrl <em>Url</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ActionImpl extends ViewPrimitiveImpl implements Action {
	/**
     * The default value of the '{@link #getAccessKey() <em>Access Key</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getAccessKey()
     * @generated
     * @ordered
     */
	protected static final String ACCESS_KEY_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getAccessKey() <em>Access Key</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getAccessKey()
     * @generated
     * @ordered
     */
	protected String accessKey = ACCESS_KEY_EDEFAULT;

	/**
     * The default value of the '{@link #getUrl() <em>Url</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getUrl()
     * @generated
     * @ordered
     */
	protected static final String URL_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getUrl() <em>Url</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getUrl()
     * @generated
     * @ordered
     */
	protected String url = URL_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected ActionImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.ACTION;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getAccessKey() {
        return accessKey;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setAccessKey(String newAccessKey) {
        String oldAccessKey = accessKey;
        accessKey = newAccessKey;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.ACTION__ACCESS_KEY, oldAccessKey, accessKey));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getUrl() {
        return url;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setUrl(String newUrl) {
        String oldUrl = url;
        url = newUrl;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.ACTION__URL, oldUrl, url));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.ACTION__ACCESS_KEY:
                return getAccessKey();
            case Model2Package.ACTION__URL:
                return getUrl();
        }
        return super.eGet(featureID, resolve, coreType);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public void eSet(int featureID, Object newValue) {
        switch (featureID) {
            case Model2Package.ACTION__ACCESS_KEY:
                setAccessKey((String)newValue);
                return;
            case Model2Package.ACTION__URL:
                setUrl((String)newValue);
                return;
        }
        super.eSet(featureID, newValue);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public void eUnset(int featureID) {
        switch (featureID) {
            case Model2Package.ACTION__ACCESS_KEY:
                setAccessKey(ACCESS_KEY_EDEFAULT);
                return;
            case Model2Package.ACTION__URL:
                setUrl(URL_EDEFAULT);
                return;
        }
        super.eUnset(featureID);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public boolean eIsSet(int featureID) {
        switch (featureID) {
            case Model2Package.ACTION__ACCESS_KEY:
                return ACCESS_KEY_EDEFAULT == null ? accessKey != null : !ACCESS_KEY_EDEFAULT.equals(accessKey);
            case Model2Package.ACTION__URL:
                return URL_EDEFAULT == null ? url != null : !URL_EDEFAULT.equals(url);
        }
        return super.eIsSet(featureID);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public String toString() {
        if (eIsProxy()) return super.toString();

        StringBuffer result = new StringBuffer(super.toString());
        result.append(" (accessKey: ");
        result.append(accessKey);
        result.append(", url: ");
        result.append(url);
        result.append(')');
        return result.toString();
    }

} //ActionImpl
