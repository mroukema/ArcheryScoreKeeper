import {join} from 'path';
import { Module } from '@nestjs/common';

// Static files
import { ServeStaticModule } from '@nestjs/serve-static';

// ArrowType
import { ArrowTypeController } from '@controller/arrow-type.controller';
import { ArrowTypeService } from '@service/arrow-type.service';

// Profile
import { ProfileController } from '@controller/profile.controller';
import { ProfileService } from '@service/profile.service';

// Target
import { TargetController } from '@controller/target.controller';
import { TargetService } from '@service/target.service';


@Module({
  imports: [
    ServeStaticModule.forRoot({
      rootPath: join(__dirname, '..', '..', 'client', 'target'),
      serveRoot: '/app',
    }),
  ],
  controllers: [ArrowTypeController, ProfileController, TargetController],
  providers: [ArrowTypeService, ProfileService, TargetService],
})
export class AppModule {}
