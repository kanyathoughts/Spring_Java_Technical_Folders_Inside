<#macro registrationLayout bodyClass="" displayInfo=false displayMessage=true displayRequiredFields=false displayWide=false showAnotherWayIfPresent=true>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" class="${properties.kcHtmlClass!}">

<head>
    <meta charset="utf-8">
    <meta name="mining-base-url" content="${realm.displayNameHtml}">
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <meta name="robots" content="noindex, nofollow">

    <#if properties.meta?has_content>
        <#list properties.meta?split(' ') as meta>
            <meta name="${meta?split('==')[0]}" content="${meta?split('==')[1]}"/>
        </#list>
    </#if>
    <title>${msg("loginTitle",(realm.displayName!''))}</title>
    <link rel="icon" href="${url.resourcesPath}/img/innowake-favicon.ico" />
    <#if properties.stylesCommon?has_content>
        <#list properties.stylesCommon?split(' ') as style>
            <link href="${url.resourcesCommonPath}/${style}" rel="stylesheet" />
        </#list>
    </#if>
    <#if properties.styles?has_content>
        <#list properties.styles?split(' ') as style>
            <link href="${url.resourcesPath}/${style}" rel="stylesheet" />
        </#list>
    </#if>
    <#if properties.scripts?has_content>
        <#list properties.scripts?split(' ') as script>
            <script src="${url.resourcesPath}/${script}" type="text/javascript"></script>
        </#list>
    </#if>
    <#if scripts??>
        <#list scripts as script>
            <script src="${script}" type="text/javascript"></script>
        </#list>
    </#if>
</head>

<body class="${properties.kcBodyClass!}">
  <div class="${properties.logoArea!}">
    <div class="${properties.logoLayer}">
        <img src="${url.resourcesPath}/img/Deloitte-Logo.svg" class="${properties.logoDeloite!}" alt="Logo Deloitte"/>
        <img src="${url.resourcesPath}/img/AppMod-Logo.svg" class="${properties.logoAppmod!}" alt="Logo App Mod"/>
        <div class="${properties.logoMining!}">
            <svg height="107px" viewBox="0 0 183 107" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
                <title>Mining vertical white</title>
                <g id="Login-(review-needed)" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                    <g id="1.1-Login-V2-Copy-12" transform="translate(-269.000000, -459.000000)">
                        <g id="Logo/iW-Logo/Mining/vertical-white" transform="translate(269.000000, 459.000000)">
                            <path d="M89.0948905,0 C66.8986129,9.70047976 56.8006864,22.9480111 57.4160584,36.2352941 C57.7799469,44.769443 63.7821068,51.5623815 75.5182482,52.0882353 C81.6568625,51.8492108 89.7587178,46.7770093 97.8043865,41.3772543 L99.4121728,40.294271 C99.5905995,40.1737509 99.7689319,40.0532174 99.947148,39.9327199 L101.014871,39.2105628 C109.542641,33.4430934 117.700129,27.9864609 123.036496,28.3088235 C134.597912,27.6439506 144.509667,37.4522547 150.189781,40.7647059 C157.858143,46.2306498 169.33746,40.8989181 169.423358,29.4411765 C169.33746,16.0063493 150.485763,16.0063493 150.189781,15.8529412 C153.010245,14.3852649 172.010879,10.6708995 178.474453,15.8529412 C185.076374,21.0108897 186.558297,33.7862244 167.160584,49.8235294 C149.443204,67.0816618 131.187255,64.8210671 123.036496,57.75 C113.664821,49.3055048 106.098823,49.7888557 101.540146,52.0882353 C95.9934497,54.3174813 28,104.098901 28,56.6176471 C28,9.68416799 86.889183,0.364719661 89.0353732,0.0107151192 Z M23,21 C7.54093885,41.8937471 10.7642458,60 10.952381,60 C-18.1376238,36.1692378 20.312787,21.776084 20.8095238,22.1142857 L23,21 Z" id="Combined-Shape-Copy" fill="#FFFFFE"></path>
                            <g id="Product-Name" transform="translate(91.000000, 77.000000)" fill="#FFFFFF" font-family="open_sanssemibold, Open Sans" font-size="22" font-weight="500">
                                <text id="Mining">
                                    <tspan x="0" y="24">${realm.displayName!}</tspan>
                                </text>
                            </g>
                        </g>
                    </g>
                </g>
            </svg>
        </div>
    </div>
    <div class="${properties.backgroungOverlay}"></div>
    <div class="${properties.gradientOverlay}"></div>
  </div>
  <div class="${properties.loginArea!}">
    <div class="${properties.loginCard!}">
      <header class="${properties.formHeaderClass!}">
        <#if realm.internationalizationEnabled  && locale.supported?size gt 1>
            <div id="kc-locale">
                <div id="kc-locale-wrapper" class="${properties.kcLocaleWrapperClass!}">
                    <div class="kc-dropdown" id="kc-locale-dropdown">
                        <a href="#" id="kc-current-locale-link">${locale.current}</a>
                        <ul>
                            <#list locale.supported as l>
                                <li class="kc-dropdown-item"><a href="${l.url}">${l.label}</a></li>
                            </#list>
                        </ul>
                    </div>
                </div>
            </div>
        </#if>
        <#if !(auth?has_content && auth.showUsername() && !auth.showResetCredentials())>
            <#if displayRequiredFields>
                <div class="${properties.kcContentWrapperClass!}">
                    <div class="${properties.kcLabelWrapperClass!} subtitle">
                        <span class="subtitle"><span class="required">*</span> ${msg("requiredFields")}</span>
                    </div>
                    <div class="col-md-10">
                        <h1 id="kc-page-title"><#nested "header"></h1>
                    </div>
                </div>
            <#else>
                <h1 id="kc-page-title"><#nested "header"></h1>
            </#if>
        <#else>
            <#if displayRequiredFields>
                <div class="${properties.kcContentWrapperClass!}">
                    <div class="${properties.kcLabelWrapperClass!} subtitle">
                        <span class="subtitle"><span class="required">*</span> ${msg("requiredFields")}</span>
                    </div>
                    <div class="col-md-10">
                        <#nested "show-username">
                        <div class="${properties.kcFormGroupClass!}">
                            <div id="kc-username">
                                <label id="kc-attempted-username">${auth.attemptedUsername}</label>
                                <a id="reset-login" href="${url.loginRestartFlowUrl}">
                                    <div class="kc-login-tooltip">
                                        <i class="${properties.kcResetFlowIcon!}"></i>
                                        <span class="kc-tooltip-text">${msg("restartLoginTooltip")}</span>
                                    </div>
                                </a>
                            </div>
                        </div>
                    </div>
                </div>
            <#else>
                <#nested "show-username">
                <div class="${properties.kcFormGroupClass!}">
                    <div id="kc-username">
                        <label id="kc-attempted-username">${auth.attemptedUsername}</label>
                        <a id="reset-login" href="${url.loginRestartFlowUrl}">
                            <div class="kc-login-tooltip">
                                <i class="${properties.kcResetFlowIcon!}"></i>
                                <span class="kc-tooltip-text">${msg("restartLoginTooltip")}</span>
                            </div>
                        </a>
                    </div>
                </div>
            </#if>
        </#if>
      </header>
      <div id="kc-content" class="${properties.formBodyClass!}">

          <#-- App-initiated actions should not see warning messages about the need to complete the action -->
          <#-- during login.                                                                               -->
          <#if displayMessage && message?has_content && (message.type != 'warning' || !isAppInitiatedAction??)>
              <div class="alert ant-form-item-has-${message.type}">
                  <#if message.type = 'success'><span class="${properties.kcFeedbackSuccessIcon!}"></span></#if>
                  <#if message.type = 'warning'><span class="${properties.kcFeedbackWarningIcon!}"></span></#if>
                  <#if message.type = 'error'><span class="${properties.kcFeedbackErrorIcon!}"></span></#if>
                  <#if message.type = 'info'><span class="${properties.kcFeedbackInfoIcon!}"></span></#if>
                  <span class="ant-form-item-explain">${kcSanitize(message.summary)?no_esc}</span>
              </div>
          </#if>

          <#nested "form">

          <#if auth?has_content && auth.showTryAnotherWayLink() && showAnotherWayIfPresent>
          <form id="kc-select-try-another-way-form" action="${url.loginAction}" method="post" <#if displayWide>class="${properties.kcContentWrapperClass!}"</#if>>
              <div <#if displayWide>class="${properties.kcFormSocialAccountContentClass!} ${properties.kcFormSocialAccountClass!}"</#if>>
                  <div class="${properties.kcFormGroupClass!}">
                    <input type="hidden" name="tryAnotherWay" value="on" />
                    <a href="#" id="try-another-way" onclick="document.forms['kc-select-try-another-way-form'].submit();return false;">${msg("doTryAnotherWay")}</a>
                  </div>
              </div>
          </form>
          </#if>

          <#if displayInfo>
              <div id="kc-info" class="${properties.kcSignUpClass!}">
                  <div id="kc-info-wrapper" class="${properties.kcInfoAreaWrapperClass!}">
                      <#nested "info">
                  </div>
              </div>
          </#if>
      </div>
    
    </div>

    <footer class="${properties.loginFooter}">
        <div>
            <p id="copyright-note-holder"></p>
            <p>Version <span id="version-holder"></span></p>
        </div>
        <div>
            <#-- Hiding Terms and Conditions and Privacy Policy links as they are not required -->
            <#-- <a href="#" class="${properties.kcButtonClass!} ${properties.buttonLink!}">${msg("termsTitle")}</a> -->
            <a href="https://cookienotice.deloitte.com" target="_blank" class="${properties.kcButtonClass!} ${properties.buttonLink!}">${msg("cookieNoticeLink")}</a>
            <a href="https://www2.deloitte.com/us/en/footerlinks1/web-privacy-notice.html" target="_blank" class="${properties.kcButtonClass!} ${properties.buttonLink!}">${msg("privacyPolicyLink")}</a>
            <a href="mailto:innowakesupport@deloitte.com" class="${properties.kcButtonClass!} ${properties.buttonLink!}">${msg("supportLink")}</a>
        </div>
    </footer>

  </div>
</body>
</html>
</#macro>
