import { Controller, Get, Param } from '@nestjs/common';
import { ProfileService } from '@service/profile.service';
import { Profile } from '@model/profile.model';

@Controller('profile')
export class ProfileController {
  constructor(private readonly profileService: ProfileService) {}

  @Get(':username')
  getProfile(@Param() username : string): Profile {
    return this.profileService.getProfile(username);
  }
}
