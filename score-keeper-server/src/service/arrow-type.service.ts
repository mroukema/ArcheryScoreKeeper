import { Injectable } from '@nestjs/common';
import { ArrowType } from '@model/arrow-type.model';

@Injectable()
export class ArrowTypeService {
  getArrowType(typeLabel : string): ArrowType {
    return new ArrowType();
  }
}
