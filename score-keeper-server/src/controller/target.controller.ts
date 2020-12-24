import { Controller, Get, Param } from '@nestjs/common';
import { TargetService } from '@service/target.service';
import { TargetType } from '@model/target-type.model';

@Controller('target')
export class TargetController {
  constructor(private readonly targetService: TargetService) {}

  @Get(':type')
  getTargetType(@Param() targetType : string): TargetType {
    return this.targetService.getTargetType(targetType);
  }
}
