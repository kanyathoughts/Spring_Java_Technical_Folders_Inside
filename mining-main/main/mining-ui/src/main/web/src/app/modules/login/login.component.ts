import { Component, OnInit } from '@angular/core';
import { Router, ActivatedRoute } from '@angular/router';
import { UntypedFormGroup, UntypedFormBuilder, Validators } from '@angular/forms';

import { environment } from '@env/environment';
import { Logger, AuthenticationService, LoginContext } from '@app/core';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { VersionControllerService } from '@innowake/mining-api-angular-client';

const log = new Logger('Login');

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html'
})
export class LoginComponent implements OnInit {
  version: string = environment.version;
  error: string | undefined;
  loginForm!: UntypedFormGroup;
  isLoading = false;
  bgImagePath = '';

  constructor(
    private router: Router,
    private route: ActivatedRoute,
    private formBuilder: UntypedFormBuilder,
    private authenticationService: AuthenticationService,
    private versionControllerService: VersionControllerService
  ) {
    this.createForm();
  }

  ngOnInit(): void {
    this.bgImagePath = getBasePath() + '/assets/login-bg.jpg';
    this.versionControllerService.getVersion().subscribe(version => {
      this.version = version['version'];
    });
    void this.router.navigate(['/']);
  }

  login(): void {
    this.isLoading = true;
    this.authenticationService.login(
      this.loginForm.value as LoginContext,
      () => {
        log.debug('successfully logged in');
        void this.router.navigate([this.route.snapshot.queryParams.redirect || '/'], { replaceUrl: true });
        this.isLoading = false;
      },
      (error: any) => {
        log.debug(`login error: ${error}`);
        this.error = error;
        this.isLoading = false;
      }
    );
  }

  private createForm() {
    this.loginForm = this.formBuilder.group({
      username: ['', Validators.required],
      password: ['', Validators.required],
      remember: true
    });
  }
}
