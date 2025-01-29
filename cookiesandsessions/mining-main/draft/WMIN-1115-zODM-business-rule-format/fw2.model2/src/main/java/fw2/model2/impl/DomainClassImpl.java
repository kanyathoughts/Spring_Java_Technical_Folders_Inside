/**
 */
package fw2.model2.impl;

import fw2.model2.DomainAttribute;
import fw2.model2.DomainClass;
import fw2.model2.Model2Package;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Domain Class</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.impl.DomainClassImpl#getSuperclass <em>Superclass</em>}</li>
 *   <li>{@link fw2.model2.impl.DomainClassImpl#getPlatformClass <em>Platform Class</em>}</li>
 *   <li>{@link fw2.model2.impl.DomainClassImpl#getAttributes <em>Attributes</em>}</li>
 *   <li>{@link fw2.model2.impl.DomainClassImpl#isAbstract <em>Abstract</em>}</li>
 *   <li>{@link fw2.model2.impl.DomainClassImpl#isPrimitive <em>Primitive</em>}</li>
 * </ul>
 *
 * @generated
 */
public class DomainClassImpl extends ModelElementImpl implements DomainClass {
	/**
     * The cached value of the '{@link #getSuperclass() <em>Superclass</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSuperclass()
     * @generated
     * @ordered
     */
	protected DomainClass superclass;

	/**
     * The cached value of the '{@link #getPlatformClass() <em>Platform Class</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPlatformClass()
     * @generated
     * @ordered
     */
	protected DomainClass platformClass;

	/**
     * The cached value of the '{@link #getAttributes() <em>Attributes</em>}' containment reference list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getAttributes()
     * @generated
     * @ordered
     */
	protected EList<DomainAttribute> attributes;

	/**
     * The default value of the '{@link #isAbstract() <em>Abstract</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isAbstract()
     * @generated
     * @ordered
     */
	protected static final boolean ABSTRACT_EDEFAULT = false;

	/**
     * The cached value of the '{@link #isAbstract() <em>Abstract</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isAbstract()
     * @generated
     * @ordered
     */
	protected boolean abstract_ = ABSTRACT_EDEFAULT;

	/**
     * The default value of the '{@link #isPrimitive() <em>Primitive</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isPrimitive()
     * @generated
     * @ordered
     */
	protected static final boolean PRIMITIVE_EDEFAULT = false;

	/**
     * The cached value of the '{@link #isPrimitive() <em>Primitive</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isPrimitive()
     * @generated
     * @ordered
     */
	protected boolean primitive = PRIMITIVE_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected DomainClassImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return Model2Package.Literals.DOMAIN_CLASS;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DomainClass getSuperclass() {
        return superclass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setSuperclass(DomainClass newSuperclass) {
        DomainClass oldSuperclass = superclass;
        superclass = newSuperclass;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DOMAIN_CLASS__SUPERCLASS, oldSuperclass, superclass));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DomainClass getPlatformClass() {
        return platformClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setPlatformClass(DomainClass newPlatformClass) {
        DomainClass oldPlatformClass = platformClass;
        platformClass = newPlatformClass;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DOMAIN_CLASS__PLATFORM_CLASS, oldPlatformClass, platformClass));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<DomainAttribute> getAttributes() {
        if (attributes == null) {
            attributes = new EObjectContainmentEList<DomainAttribute>(DomainAttribute.class, this, Model2Package.DOMAIN_CLASS__ATTRIBUTES);
        }
        return attributes;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isAbstract() {
        return abstract_;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setAbstract(boolean newAbstract) {
        boolean oldAbstract = abstract_;
        abstract_ = newAbstract;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DOMAIN_CLASS__ABSTRACT, oldAbstract, abstract_));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isPrimitive() {
        return primitive;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setPrimitive(boolean newPrimitive) {
        boolean oldPrimitive = primitive;
        primitive = newPrimitive;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Model2Package.DOMAIN_CLASS__PRIMITIVE, oldPrimitive, primitive));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Model2Package.DOMAIN_CLASS__ATTRIBUTES:
                return ((InternalEList<?>)getAttributes()).basicRemove(otherEnd, msgs);
        }
        return super.eInverseRemove(otherEnd, featureID, msgs);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Model2Package.DOMAIN_CLASS__SUPERCLASS:
                return getSuperclass();
            case Model2Package.DOMAIN_CLASS__PLATFORM_CLASS:
                return getPlatformClass();
            case Model2Package.DOMAIN_CLASS__ATTRIBUTES:
                return getAttributes();
            case Model2Package.DOMAIN_CLASS__ABSTRACT:
                return isAbstract();
            case Model2Package.DOMAIN_CLASS__PRIMITIVE:
                return isPrimitive();
        }
        return super.eGet(featureID, resolve, coreType);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
        switch (featureID) {
            case Model2Package.DOMAIN_CLASS__SUPERCLASS:
                setSuperclass((DomainClass)newValue);
                return;
            case Model2Package.DOMAIN_CLASS__PLATFORM_CLASS:
                setPlatformClass((DomainClass)newValue);
                return;
            case Model2Package.DOMAIN_CLASS__ATTRIBUTES:
                getAttributes().clear();
                getAttributes().addAll((Collection<? extends DomainAttribute>)newValue);
                return;
            case Model2Package.DOMAIN_CLASS__ABSTRACT:
                setAbstract((Boolean)newValue);
                return;
            case Model2Package.DOMAIN_CLASS__PRIMITIVE:
                setPrimitive((Boolean)newValue);
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
            case Model2Package.DOMAIN_CLASS__SUPERCLASS:
                setSuperclass((DomainClass)null);
                return;
            case Model2Package.DOMAIN_CLASS__PLATFORM_CLASS:
                setPlatformClass((DomainClass)null);
                return;
            case Model2Package.DOMAIN_CLASS__ATTRIBUTES:
                getAttributes().clear();
                return;
            case Model2Package.DOMAIN_CLASS__ABSTRACT:
                setAbstract(ABSTRACT_EDEFAULT);
                return;
            case Model2Package.DOMAIN_CLASS__PRIMITIVE:
                setPrimitive(PRIMITIVE_EDEFAULT);
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
            case Model2Package.DOMAIN_CLASS__SUPERCLASS:
                return superclass != null;
            case Model2Package.DOMAIN_CLASS__PLATFORM_CLASS:
                return platformClass != null;
            case Model2Package.DOMAIN_CLASS__ATTRIBUTES:
                return attributes != null && !attributes.isEmpty();
            case Model2Package.DOMAIN_CLASS__ABSTRACT:
                return abstract_ != ABSTRACT_EDEFAULT;
            case Model2Package.DOMAIN_CLASS__PRIMITIVE:
                return primitive != PRIMITIVE_EDEFAULT;
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
        result.append(" (abstract: ");
        result.append(abstract_);
        result.append(", primitive: ");
        result.append(primitive);
        result.append(')');
        return result.toString();
    }

} //DomainClassImpl
