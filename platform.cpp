#include "platform.h"
#include "config.h"

#include <math.h>
#include <stdarg.h>

OBJECT_IMPL(CPlatformer, Component);
OBJECT_PROPERTY(CPlatformer, w);
OBJECT_PROPERTY(CPlatformer, h);
OBJECT_PROPERTY(CPlatformer, density);
OBJECT_PROPERTY(CPlatformer, parent);

CPlatformer::CPlatformer(void* go)
  : Component((GO*)go, PRIORITY_THINK), w(10), h(10),
    density(1), fixture(NULL), parent(NULL) {
  this->go->body->SetType(b2_dynamicBody);
}

CPlatformer::~CPlatformer() {
  go->body->DestroyFixture(fixture);
  go->body->SetType(b2_staticBody);
}

void CPlatformer::init() {
  b2PolygonShape box;
  box.SetAsBox((w/2)/BSCALE, (h/2)/BSCALE);
  fixture = go->body->CreateFixture(&box, density);
  fixture->SetUserData(this);
}

void CPlatformer::update(float dt) {
  // try to find a contact with our fixture
  b2ContactEdge* node = go->body->GetContactList();
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
        parent = other;
        return;
      }
    }
    node = node->next;
  }
}
