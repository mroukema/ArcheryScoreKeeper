import { Injectable } from '@nestjs/common';
import { TargetType } from '@model/target-type.model'

@Injectable()
export class TargetService {
  constructor() {}

  getTargetType(targetName : string) : TargetType {
    return new TargetType();
  }
}
