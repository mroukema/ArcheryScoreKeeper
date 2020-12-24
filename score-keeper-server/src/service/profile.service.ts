import { Injectable } from '@nestjs/common';
import { Profile } from '@model/profile.model';

@Injectable()
export class ProfileService {
  getProfile(username : string): Profile {
    return new Profile('mroukema');
  }
}
