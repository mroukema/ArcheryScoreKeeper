import { Controller, Get, Param } from '@nestjs/common';
import { ArrowTypeService } from '@service/arrow-type.service';
import { ArrowType } from '@model/arrow-type.model';

@Controller('arrow')
export class ArrowTypeController {
  constructor(private readonly arrowTypeService: ArrowTypeService) {}

  @Get(':typeLabel')
  getProfile(@Param() typeLabel : string): ArrowType {
    return this.arrowTypeService.getArrowType(typeLabel);
  }
}
