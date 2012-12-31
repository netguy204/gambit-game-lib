#include "platform.h"
#include "config.h"

#include <math.h>
#include <stdarg.h>

OBJECT_IMPL(CPlatformer, Component);
OBJECT_PROPERTY(CPlatformer, w);
OBJECT_PROPERTY(CPlatformer, h);
OBJECT_PROPERTY(CPlatformer, density);
OBJECT_PROPERTY(CPlatformer, friction);
OBJECT_PROPERTY(CPlatformer, parent);

CPlatformer::CPlatformer(void* go)
  : Component((GO*)go, PRIORITY_THINK), w(10), h(10),
    density(1), friction(0.2), fixture(NULL), parent(NULL) {
  this->go->body->SetType(b2_dynamicBody);
}

CPlatformer::~CPlatformer() {
  go->body->DestroyFixture(fixture);
  go->body->SetType(b2_staticBody);
}

void CPlatformer::init() {
  b2FixtureDef fixtureDef;
  fixtureDef.friction = friction;
  fixtureDef.density = density;

  b2PolygonShape box;
  box.SetAsBox((w/2)/BSCALE, (h/2)/BSCALE);
  fixtureDef.shape = &box;

  fixture = go->body->CreateFixture(&fixtureDef);
  fixture->SetUserData(this);
}

void CPlatformer::update(float dt) {
  // try to find a contact with our fixture
  b2ContactEdge* node = go->body->GetContactList();
  GO* old_parent = parent;
  Vector_ ourpos;
  go->pos(&ourpos);

  parent = NULL;

  while(node) {
    if(node->contact->IsTouching() &&
       (node->contact->GetFixtureA() == fixture ||
        node->contact->GetFixtureB() == fixture)) {

      // need to ensure normal is up
      GO* other = (GO*)node->other->GetUserData();
      b2WorldManifold manifold;
      node->contact->GetWorldManifold(&manifold);

      // normal goes from A to B so we need to be careful
      if((node->contact->GetFixtureA() == fixture && manifold.normal.y < 0)
         || manifold.normal.y > 0) {

        // normal is pointing the right way but the contact must be
        // below us
        if((manifold.points[0].y*BSCALE) < ourpos.y) {
          parent = other;
          break;
        }
      }
    }
    node = node->next;
  }

  if(parent != old_parent) {
    go->send_message(go->create_message(MESSAGE_PARENT_CHANGE));
  }
}
