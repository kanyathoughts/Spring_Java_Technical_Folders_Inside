import { KeycloakMiningRole } from './keycloak-mining-role.interface';

export interface KeycloakAuth {
  now: string,
  subject: string,
  access_token_issued: string,
  access_token_expires: string,
  session_id: string,
  session_created: string,
  session_accessed: string,
  session_expires: string,
  user_logon_name: string,
  user_full_name: string,
  user_given_name: string,
  user_family_name: string,
  user_email: string,
  mining_roles: KeycloakMiningRole[]
}
