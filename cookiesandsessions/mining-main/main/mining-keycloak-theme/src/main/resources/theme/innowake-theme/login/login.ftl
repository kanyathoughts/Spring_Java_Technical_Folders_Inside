<#import "template.ftl" as layout>
<@layout.registrationLayout displayInfo=social.displayInfo displayWide=(realm.password && social.providers??); section>
    <#if section = "header">
        ${msg("doLogIn")}
    <#elseif section = "form">
    <div id="kc-form" <#if realm.password && social.providers??>class="${properties.kcContentWrapperClass!}"</#if>>
      <div id="kc-form-wrapper" <#if realm.password && social.providers??>class="${properties.kcFormSocialAccountContentClass!} ${properties.kcFormSocialAccountClass!}"</#if>>
        <#if realm.password>
            <form id="kc-form-login" onsubmit="login.disabled = true; return true;" action="${url.loginAction}" method="post">
                <div class="${properties.kcFormGroupClass!}">
                    <div class="ant-input-affix-wrapper">
                        <span class="ant-input-prefix" type="prefix">
                            <i class="anticon anticon-user">
                                <svg viewBox="64 64 896 896" focusable="false" fill="currentColor" width="1em" height="1em" data-icon="user" aria-hidden="true"><path d="M858.5 763.6a374 374 0 00-80.6-119.5 375.63 375.63 0 00-119.5-80.6c-.4-.2-.8-.3-1.2-.5C719.5 518 760 444.7 760 362c0-137-111-248-248-248S264 225 264 362c0 82.7 40.5 156 102.8 201.1-.4.2-.8.3-1.2.5-44.8 18.9-85 46-119.5 80.6a375.63 375.63 0 00-80.6 119.5A371.7 371.7 0 00136 901.8a8 8 0 008 8.2h60c4.4 0 7.9-3.5 8-7.8 2-77.2 33-149.5 87.8-204.3 56.7-56.7 132-87.9 212.2-87.9s155.5 31.2 212.2 87.9C779 752.7 810 825 812 902.2c.1 4.4 3.6 7.8 8 7.8h60a8 8 0 008-8.2c-1-47.8-10.9-94.3-29.5-138.2zM512 534c-45.9 0-89.1-17.9-121.6-50.4S340 407.9 340 362c0-45.9 17.9-89.1 50.4-121.6S466.1 190 512 190s89.1 17.9 121.6 50.4S684 316.1 684 362c0 45.9-17.9 89.1-50.4 121.6S557.9 534 512 534z"></path></svg>
                            </i>
                        </span>
                        <#if usernameEditDisabled??>
                            <input tabindex="1" id="username" class="${properties.kcInputClass!}" name="username" value="${(login.username!'')}" placeholder="<#if !realm.loginWithEmailAllowed || !realm.registrationEmailAsUsername>${msg('username')}<#else>${msg('email')}</#if>" type="text" disabled />
                        <#else>
                            <input tabindex="1" id="username" class="${properties.kcInputClass!}" name="username" value="${(login.username!'')}" placeholder="<#if !realm.loginWithEmailAllowed || !realm.registrationEmailAsUsername>${msg('username')}<#else>${msg('email')}</#if>" type="text" autofocus autocomplete="off" />
                        </#if>
                    </div>
                </div>

                <div class="${properties.kcFormGroupClass!}">
                    <div class="ant-input-affix-wrapper">
                        <span class="ant-input-prefix" type="prefix">
                            <i class="anticon anticon-lock">
                                <svg viewBox="64 64 896 896" focusable="false" fill="currentColor" width="1em" height="1em" data-icon="lock" aria-hidden="true"><path d="M832 464h-68V240c0-70.7-57.3-128-128-128H388c-70.7 0-128 57.3-128 128v224h-68c-17.7 0-32 14.3-32 32v384c0 17.7 14.3 32 32 32h640c17.7 0 32-14.3 32-32V496c0-17.7-14.3-32-32-32zM332 240c0-30.9 25.1-56 56-56h248c30.9 0 56 25.1 56 56v224H332V240zm460 600H232V536h560v304zM484 701v53c0 4.4 3.6 8 8 8h40c4.4 0 8-3.6 8-8v-53a48.01 48.01 0 10-56 0z"></path></svg>
                            </i>
                        </span>
                        <input tabindex="2" id="password" class="${properties.kcInputClass!}" placeholder="${msg('password')}" name="password" type="password" autocomplete="off" />
                    </div>
                </div>

                <div class="${properties.kcFormGroupClass!} ${properties.kcFormSettingClass!}">
                    <div id="kc-form-options">
                        <#if realm.rememberMe && !usernameEditDisabled??>
                            <div class="checkbox">
                                
                                <#if login.rememberMe??>
                                    <label class="${properties.checkboxWrapper!} ant-checkbox-wrapper-checked">
                                        <span class="${properties.checkboxDiv!} ant-checkbox-checked">
                                            <input tabindex="3" id="rememberMe" name="rememberMe" type="checkbox" class="${properties.checkboxInput!}" checked>
                                            <span class="ant-checkbox-inner"></span>
                                        </span>
                                        <span>${msg("rememberMe")}</span>
                                    </label>
                                <#else>
                                    <label id="checkboxWrapper" class="${properties.checkboxWrapper!}">
                                        <span id="checkboxSpan" class="${properties.checkboxDiv!}">
                                            <input tabindex="3" id="rememberMe" name="rememberMe" type="checkbox" class="${properties.checkboxInput!}" onclick="togleStyle();">
                                            <span class="ant-checkbox-inner"></span>
                                        </span>
                                        <span>${msg("rememberMe")}</span>
                                    </label>
                                </#if>
                                <script>
                                    function togleStyle() {
                                        console.log(document.getElementById('rememberMe').checked)
                                        if (document.getElementById('rememberMe').checked) 
                                        {
                                            document.getElementById('checkboxWrapper').classList.add("ant-checkbox-wrapper-checked");
                                            document.getElementById('checkboxSpan').classList.add("ant-checkbox-checked");
                                        } else {
                                            document.getElementById('checkboxWrapper').classList.remove("ant-checkbox-wrapper-checked");
                                            document.getElementById('checkboxSpan').classList.remove("ant-checkbox-checked");
                                        }
                                    }
                                </script>
                            </div>
                        </#if>
                        </div>
                        <div class="${properties.kcFormOptionsWrapperClass!}">
                            <#if realm.resetPasswordAllowed>
                                <span><a tabindex="5" href="${url.loginResetCredentialsUrl}">${msg("doForgotPassword")}</a></span>
                            </#if>
                        </div>

                  </div>

                  <div id="kc-form-buttons" class="${properties.kcFormGroupClass!}">
                      <input type="hidden" id="id-hidden-input" name="credentialId" <#if auth.selectedCredential?has_content>value="${auth.selectedCredential}"</#if>/>
                      <button tabindex="4" class="${properties.kcButtonClass!} ${properties.kcButtonPrimaryClass!} ${properties.kcButtonBlockClass!} ${properties.kcButtonLargeClass!}" name="login" id="kc-login" type="submit">${msg("doLogIn")}</button>
                  </div>
            </form>
        </#if>
        </div>
        <#if realm.password && social.providers??>
            <div id="kc-social-providers" class="${properties.kcFormSocialAccountContentClass!} ${properties.kcFormSocialAccountClass!}">
                <ul class="${properties.kcFormSocialAccountListClass!} <#if social.providers?size gt 4>${properties.kcFormSocialAccountDoubleListClass!}</#if>">
                    <#list social.providers as p>
                        <li class="${properties.kcFormSocialAccountListLinkClass!}"><a href="${p.loginUrl}" id="zocial-${p.alias}" class="zocial ${p.providerId}"> <span>${p.displayName}</span></a></li>
                    </#list>
                </ul>
            </div>
        </#if>
      </div>
    <#elseif section = "info" >
        <#if realm.password && realm.registrationAllowed && !registrationDisabled??>
            <div id="kc-registration">
                <span>${msg("noAccount")} <a tabindex="6" href="${url.registrationUrl}">${msg("doRegister")}</a></span>
            </div>
        </#if>
    </#if>

</@layout.registrationLayout>
